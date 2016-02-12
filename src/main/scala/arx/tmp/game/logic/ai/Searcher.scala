package arx.tmp.game.logic.ai

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/6/13
 * Time: 9:43 AM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.core.FibonacciHeap
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.MutableVoxelCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.voxelregions.VoxelRegion

import scala.collection.mutable

object Searcher {

	object AStarSearchNodeSentinel extends AStarSearchNode(VoxelCoord.Sentinel,Float.MaxValue,Float.MaxValue,null)

	case class AllPathsQuery (
		from : VoxelCoord,
		voxelDimensions : Vec3i,
		maxCost : Int,
		searchLimitFunction : (VoxelCoord) => Boolean,
		heuristic : (VoxelCoord,VoxelCoord) => Float,
		obstructionFunction : (VoxelCoord) => Boolean,
		isSupportedFunction : (VoxelCoord) => Boolean,
		allowUnsupportedEndpoints : Boolean
	)

	def allPathsFor ( params : AllPathsQuery ) : mutable.HashMap[VoxelCoord,SearchNode] = {
		import params._
		if ( from.isSentinel ) { return new mutable.HashMap[VoxelCoord,SearchNode]() }

		val heap = new FibonacciHeap[SearchNode]
		heap.enqueue( SearchNode(from,0,null) )

		val closedSet = new mutable.HashSet[VoxelCoord]
		val resultMap = new mutable.HashMap[VoxelCoord,SearchNode]

		//We're doing this to minimize the number of unnecessary allocations we do, though
		//who knows if that's actually liable to be helping anything, in truth
		var _tmpv = MutableVoxelCoord(0,0,0)

		while ( heap.nonEmpty && heap.peek.g <= maxCost ) {
			val node = heap.dequeue()

			if ( ! closedSet.contains( node.v ) ) {
				//test if this is a valid space to occupy
				if ( ! obstructionFunction(node.v) || node.parent == null ) {
					closedSet.add(node.v)

					val supported = isSupportedFunction(node.v)
					val previousSupported = node.parent == null || isSupportedFunction( node.parent.v )

					//Only add it to the results if it is a location one could actually end up standing
					if ( supported || allowUnsupportedEndpoints ) { resultMap(node.v) = node }

					var q = 0; while ( q < 6 ) {
						val dx = node.v.x + cardinalsX(q)
						val dy = node.v.y + cardinalsY(q)
						val dz = node.v.z + cardinalsZ(q)
						_tmpv.set(dx,dy,dz)
						val nextSupported = isSupportedFunction( _tmpv )

						if ( searchLimitFunction(_tmpv) ) {
							//If we are supported on the ground we can jump, and if we have just jumped we can
							//land or fall, but cannot jump from midair
							if ( supported ||
								(previousSupported && (cardinalsZ(q) == 0 && nextSupported)) ||
								cardinalsZ(q) < 0 )
							{
								heap.enqueue( SearchNode( _tmpv , node.g + 1 , node ) )
								_tmpv = MutableVoxelCoord(0,0,0)
							}
						}
						q += 1}
				}
			}
		}

		resultMap
	}

	@inline
	def defaultHeuristic ( from : VoxelCoord, to : VoxelCoord ) = //absf(to.x - from.x) + absf(to.y - from.y) + absf(to.z - from.z)
		sqrtf( (from.x-to.x)*(from.x-to.x) + (from.y-to.y)*(from.y-to.y) + (from.z-to.z)*(from.z-to.z) )

	case class PathQuery (
		from : VoxelCoord,
		to : VoxelRegion,
		voxelDimensions : ReadVec3i,
		maxCost : Int,
		moveCostFunction : (VoxelCoord,VoxelCoord) => Float,
		searchLimitFunction : (VoxelCoord) => Boolean,
		heuristic : (VoxelCoord,VoxelCoord) => Float,
		obstructionFunction : (VoxelCoord) => Boolean,
		isSupportedFunction : (VoxelCoord) => Boolean,
		allowUnsupportedEndpoint : Boolean = false
	) {
		var floodQuery = false
		var finishedCriteria : (VoxelCoord) => Boolean = (v) => false
	}

	def pathTo (parameters : PathQuery) : Option[List[VoxelCoord]] =
	{
		import parameters._


		val firstTo = to.head
		var validDestExists = false
		to.foreachUnsafe(v => {
			if (isSupportedFunction(v) && ! obstructionFunction(v)) {
				validDestExists = true
				// if our from is a valid destination we are already finished
				if (v == from) {
					return Some(Nil)
				}
			}
		})

		if ( ! validDestExists ) { return None }


		val heap = new FibonacciHeap[AStarSearchNode]
		heap.enqueue( AStarSearchNode(from,0,heuristic(from,firstTo),null) )

		val closedSet = new mutable.HashSet[VoxelCoord]
		val resultMap = new mutable.HashMap[VoxelCoord,AStarSearchNode]

		while ( heap.nonEmpty && heap.peek.g <= maxCost ) {
			var node = heap.dequeue()

			if (to.contains(node.v)) {
				if (isSupportedFunction(node.v) && ! obstructionFunction(node.v)) {
					return Some(node.path)
				}
			}

			if ( ! closedSet.contains( node.v ) ) {
				//test if this is a valid space to occupy
				closedSet.add(node.v)
				if ( ! obstructionFunction(node.v) || node.parent == null ) {
					val supported = isSupportedFunction(node.v)
					val previousSupported = node.parent == null || isSupportedFunction(node.parent.v)

					//Only add it to the results if it is a location one could actually end up standing
					if ( resultMap.getOrElse(node.v,AStarSearchNodeSentinel).g > node.g ) {
						if ( resultMap.contains(node.v) ) {
							val tmpNode = resultMap(node.v)
							tmpNode.g = node.g
							tmpNode.parent = node.parent
							node = tmpNode
						} else {
							resultMap(node.v) = node
						}

						var q = 0; while ( q < 6 ) {
							val dx = node.v.x + cardinalsX(q)
							val dy = node.v.y + cardinalsY(q)
							val dz = node.v.z + cardinalsZ(q)
							val newV = VoxelCoord(dx,dy,dz)
							val nextSupported = isSupportedFunction( newV )

							if ( searchLimitFunction(newV) ) {
								//If we are supported on the ground we can jump, and if we have just jumped we can
								//land or fall, but cannot jump from midair
								if ( supported ||
									(previousSupported && (cardinalsZ(q) == 0 && nextSupported)) ||
									cardinalsZ(q) < 0 )
								{
									val cost = moveCostFunction( node.v , newV )
									heap.enqueue( AStarSearchNode( newV , node.g + cost , heuristic(newV,firstTo) , node ) )
								}
							}
							q += 1}
					}
				}
			}
		}

		None
	}
}
