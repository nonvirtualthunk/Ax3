package arx.eldr.game.logic.ai.search

/**
 * TODO: Add javadoc
 */

import java.util

import arx.Prelude._
import arx.application.Noto
import arx.application.SimpleLoggingLevelProvider
import arx.axistential.game.logic.ai.RawSearchNode
import arx.core.FibonacciHeap
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.vec._
import arx.core.vec.coordinates.MutableVoxelCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.eldr.game.world.data.DebugData
import arx.engine.world.World
import scala.collection.mutable
import scalaxy.loops._

object Searcher {
	protected val searchers = new ThreadLocal[Searcher] {
		override def initialValue(): Searcher = new Searcher
	}

	def pathTo( query : PathQuery ) = searchers.get.pathTo(query)
	def allPaths( query : PathQuery ) = searchers.get.allPaths(query)
}

class PathQuery(val world : World, val from : VoxelCoord, val to : VoxelRegion) {
	var floodQuery = false
	var maxCost : Int = 500
	var allowUnsupportedEndpoint = false
	var moveCostFunction : (VoxelCoord,VoxelCoord,Int,Int) => Float = (v1,v2,c,j) => 1.0f
	var searchLimitFunction : (VoxelCoord) => Boolean = (v) => true
	var obstructionFunction : (VoxelCoord) => Boolean = (v) => false
	var isSupportedFunction : (VoxelCoord) => Boolean = (v) => true
	var heuristicFunction : (VoxelCoord,VoxelCoord) => Float = (v1,v2) => distance(v1,v2)
	var finishedCriteria : (VoxelCoord) => Boolean = (v) => false
	var climbLimit = 3
	var jumpLimit = 1
}

class Searcher {
	private[this] var activeNodes = 0
	private[this] var nodePool = fillArray(4096)(new RawSearchNode(_))


	protected def createNode () = {
		if ( activeNodes >= nodePool.length ) {
			val oldPool = nodePool
			nodePool = Array.ofDim[RawSearchNode](nodePool.length * 2)
			System.arraycopy(oldPool,0,nodePool,0,oldPool.length)
			var q = oldPool.length; while ( q < nodePool.length ) {
				nodePool(q) = new RawSearchNode(q)
				q += 1}

		}
		activeNodes += 1
		nodePool(activeNodes-1)
	}


	protected def returnNode ( node : RawSearchNode ) {
		activeNodes -= 1
		val endNode = nodePool(activeNodes)
		endNode._index = node._index
		nodePool(node._index) = endNode
		node._index = activeNodes
		nodePool(activeNodes) = node
	}

	protected def clearNodes () {
		activeNodes = 0
	}

	protected def path ( node : RawSearchNode ) : List[VoxelCoord] = {
		if ( node.parent == null ) {
			List(VoxelCoord(node.v))
		} else {
			path(node.parent) ::: VoxelCoord(node.v) :: Nil
		}
	}


	def allPaths ( parameters : PathQuery ) : Traversable[VoxelCoord] = {
		val res = new mutable.ListBuffer[VoxelCoord]
		subSearch(parameters,res)
		res
	}
	def pathTo ( parameters : PathQuery) : Option[List[VoxelCoord]] = subSearch(parameters,null)
	def subSearch (parameters : PathQuery, resultSet : mutable.ListBuffer[VoxelCoord]) : Option[List[VoxelCoord]] =
	{
		import parameters._
		
		val allPathsSearch = resultSet != null
		if (to.isEmpty) {
			Noto.warn("[Searcher] No path found, the <to> set was empty")
			return None
		} else if (obstructionFunction(from)) {
			Noto.fine(PathfindingLogging, "[Searcher] No path found, the <from> was obstructed")
			return None
		}

		//Reset our node pool, making all nodes accessible
		clearNodes()

		val noParentStandin = createNode().init(0,0,0,0,0,0,0,null)

		//0-3 cardinals, 4-7 diagonals, 8-9 verticals
		val pathingCardinalsX = Array(1, 0,-1, 0, 1, 1,-1,-1, 0, 0)
		val pathingCardinalsY = Array(0, 1, 0,-1, 1,-1, 1,-1, 0, 0)
		val pathingCardinalsZ = Array(0, 0, 0, 0, 0, 0, 0, 0,-1, 1)



		val hasValidDest = to.existsUnsafe(v => (allowUnsupportedEndpoint || isSupportedFunction(v)) && ! obstructionFunction(v))
		val closestTo = to.minBy(t => distance(t,from))

		val H = 1

		if ( ! allPathsSearch && ! floodQuery && to.contains(from) ) {
			Noto.fine(PathfindingLogging, "[Searcher] Search early exited, already at destination")
			return Some(Nil)
		}
		if ( ! allPathsSearch && ! floodQuery && !hasValidDest ) {
			Noto.fine(PathfindingLogging, "[Searcher] No path found, no valid destinations")
			return None
		}

		val heap = new FibonacciHeap[RawSearchNode]
		heap.enqueue( createNode().init(from.x,from.y,from.z,0,0,0,heuristicFunction(from,closestTo),null) )

		val closedSet = new mutable.HashSet[RawSearchNode]
		val openSet = new util.HashMap[RawSearchNode,FibonacciHeap.Node[RawSearchNode]]

		val tmpV = MutableVoxelCoord(0,0,0)
		val obsV = MutableVoxelCoord(0,0,0)

		while ( heap.nonEmpty && heap.peek.g <= maxCost ) {
			val node = heap.dequeue()

			// Check to see if we have found a solution
			if (isSupportedFunction(node.v) && ! obstructionFunction(node.v)) {
				if (floodQuery) {
					if (finishedCriteria(node.v)) {
						return Some(path(node))
					}
				} else {
					if (to.contains(node.v)) {
						val finishedPath = path(node)
						return Some(finishedPath)
					}
				}
			}

			// otherwise check that we haven't already examined this node before
			if ( ! closedSet.contains( node ) ) {
				// and if we haven't, add it to the closed set, remove it from the open set
				closedSet.add(node)
				openSet.remove(node)
				// if we are doing an all-paths search, add this node to the result set, because we have the closed set check, this happens only once per voxel
				if ( allPathsSearch ) {
					if ( allowUnsupportedEndpoint || (node.c == 0 && node.j == 0) ) {
						resultSet.append( VoxelCoord(node.v) )
					}
				}

				// we need to keep track of whether or not there is an adjacent obstruction to allow for climbing (can't climb empty air)
				var adjacentObstruction = false
				val parentOE = if (node.parent == null) { noParentStandin } else { node.parent }
				val parentV = parentOE.v
				// loop through all adjacencies, 4 horizontal cardinals, 4 horizontal diagonals, then the 2 vertical cardinals
				var q = 0; while ( q < 10 ) {
					tmpV.x = node.v.x + pathingCardinalsX(q)
					tmpV.y = node.v.y + pathingCardinalsY(q)
					tmpV.z = node.v.z + pathingCardinalsZ(q)

					// Don't advance here if it is outside the limits of our search (if we are, say, bounded to a certain region)
					// or if it is a return to our parent node (unless we are returning with a lower climb or jump count, this
					// would be climbing a few voxels to a ledge, then returning to climbing for example, and is valid)
					if ( (! tmpV.fastEquals(parentV) || node.c < parentOE.c || node.j < parentOE.j) && searchLimitFunction(tmpV) ) {
						var obstruction = false
						obsV.x = tmpV.x
						obsV.y = tmpV.y
						// Check for obstructions in every voxel from our current examination point up to the entity's height
						// TODO: this should check the full bounds of the entity, not all entities are 1x1xN
						var h = if (q == 9) { H - 1 } else { 0 }
						val endH = if (q == 8) { 1 } else { H }
						while ( h < endH && ! obstruction ) {
							obsV.z = tmpV.z + h
							obstruction ||= obstructionFunction(obsV)

							if ( q >= 4 && q < 8 ) { //Diagonals are only allowed when the two adjacents are open
								obsV.x = node.v.x
								obstruction ||= obstructionFunction(obsV)
								obsV.x = tmpV.x
								obsV.y = node.v.y
								obstruction ||= obstructionFunction(obsV)
								obsV.y = tmpV.y

							}
							h += 1}

						// If no obstruction was found this could be a valid movement point
						if ( ! obstruction ) {
							// TODO: Check that both adjacents are supported when looking at diagonals
							val nextSupported = isSupportedFunction( tmpV )

							//We can continue to this node if :
							//1) it is supported (that is, on firm ground)
							//2) not supported, but moving vertically with an adjacent wall, up to climbLimit distance
							//3) no jump has yet been made and not currently climbing (that is, no jump from a climb)
							//	  3.a) one jump has been made, and we are moving downward
							if ( nextSupported ||
								(adjacentObstruction && pathingCardinalsZ(q) != 0 && node.c < climbLimit) ||
								(node.c == 0 && (node.j == 0 || (node.j <= 2 && pathingCardinalsZ(q) < 0)))
							) {
								val newC = if ( ! nextSupported && adjacentObstruction && pathingCardinalsZ(q) != 0 ) { node.c + 1 } else { 0 }
								val newJ = if ( ! nextSupported && ! adjacentObstruction ) { node.j + 1 } else { 0 }
								val cost = moveCostFunction( node.v , tmpV , newC , newJ )
								val rawH = heuristicFunction(tmpV,closestTo)
								val newNode = createNode().init( tmpV.x,tmpV.y,tmpV.z, newC, newJ, node.g + cost , rawH , node )

								val existingResult : FibonacciHeap.Node[RawSearchNode] = openSet.get(node)
								if ( existingResult == null || existingResult.data.g > node.g ) {
									if ( existingResult != null ) {
										existingResult.data.g = node.g
										existingResult.data.parent = node.parent
										heap.updateKey(existingResult)
										returnNode(newNode)
									} else {
										val fibNode = heap.enqueue( newNode )
										openSet.put(newNode,fibNode)
									}
								}
							}
						} else {
							adjacentObstruction ||= (q < 4)
						}
					}
					q += 1
				}
			}
		}

		val closest = closedSet.minBy(s => s.h)
		val DD = world[DebugData]
		if (DD.active) {
			val bestPath = path(closest)
			//			DD.voxelGroups += "PathDebug" -> DebugData.VoxelGroup(Vec4f(0.2f,0.8f,0.2f,0.3f),closedSet.filter(n => n.c == 1).map(n => VoxelCoord(n.parent.v)).toSet.take(5000))
			DD.voxelGroups += "PathDebug" -> DebugData.VoxelGroup(Vec4f(0.2f,0.8f,0.2f,0.3f),bestPath.toSet)
		}

		None
	}
}

object PathfindingLogging extends SimpleLoggingLevelProvider