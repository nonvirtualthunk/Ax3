package arx.axistential.game.world.generation.generators.hydrological

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/10/13
 * Time: 9:21 AM
 */

import arx.axistential.game.archetypes.Material
import arx.axistential.game.components.fluid.FinitePushFluidComputor
import arx.axistential.game.data.world._
import arx.axistential.game.world.generation.WorldGenerator.Phase
import arx.axistential.game.world.generation.WorldGenerator.Stage
import arx.axistential.game.world.generation.Progress
import arx.axistential.game.world.generation.TWorldGenerator
import arx.axistential.game.world.generation.WorldGenerator
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.ReadVec2i
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec2i
import arx.core.FibonacciHeap
import arx.core.THasSortKey
import arx.engine.data.TWorldAuxData
import arx.engine.world.World
import arx.tmp.game.logic.ai.GeneralizedSearcher

import scala.collection.mutable
import scalaxy.loops._

class RiverGenerator extends TWorldGenerator {
	
	case class RiverNode (v:VoxelCoord,g:Float,direction:Int) extends THasSortKey {
		def sortKey: Float = v.z + g * 0.05f
	}

	def flowRiver(world: World, river: River){
		val minimumZ = world.worldRegionAsSpatialRegion.minZ + 100
		val fluidData = world.auxData[FluidData]

		val fluidLevel = fluidData.fluidLevel
		val fluidType = fluidData.fluidType
		val terrain = world.aux[TerrainData].materialGrid
		val SnowLine = world.aux[ScaleData].SnowLine.inVoxels.toInt
		//
		val typeByte = world.aux[TerrainData].materialMapping(Material.materialWithName("water"))

		val threeSource = VoxelCoord(river.source.x,river.source.y,VoxelCoord.Center.z + world.aux[TopologicalData].heightmap(river.source.xy)+1)

		var maxStepsRemaining = 1000000000
		val iterations = 4
		for ( iter <- 0 to iterations optimized ) {
			val cardinals : Array[ReadVec3i] = if ( iter == iterations ) {
				Cardinals.cardinals
			} else {
				Cardinals.expandedCardinals.filter( c => c.z == 0 || (c.x == 0 && c.y == 0) )
			}
			val cardinalLengths = cardinals.map( _.length )

			val Q = new FibonacciHeap[RiverNode]
			Q.enqueue( RiverNode(threeSource,0,arx.Prelude.Center) )

			val closed = new mutable.HashSet[VoxelCoord]

			var chosenNodes : List[RiverNode] = List[RiverNode]()
			while ( Q.nonEmpty && Q.peek.v.z > minimumZ && maxStepsRemaining > 0 ) {
				maxStepsRemaining -= 1
				val node = Q.dequeue()
				chosenNodes ::= node

				for ( q <- 0 until cardinals.length optimized ) {
					val av = node.v + cardinals(q)

						if ( ! closed.contains(av) && TerrainByteUtils.isFluidPassable(terrain(av) ) ) {
							closed.add(av)

							Q.enqueue( RiverNode(av,node.g+cardinalLengths(q),q) )
						}
//					}
				}
			}

			for ( node <- chosenNodes ; if node.v.z - VoxelCoord.Center.z < SnowLine ) {
				if ( iter == iterations ) {
//					fluidType(node.v) = typeByte
//					fluidLevel(node.v) = -1.toShort
				} else {
					val dv = node.v + Cardinals.cardinals(Down)
					if ( ! TerrainByteUtils.isFluidPassable( terrain(dv) ) ) {
						for ( q <- 0 until 6 optimized ) {
							if ( q != Top ) {
								val adj = node.v + Cardinals.cardinals(q)
								terrain(adj) = 0.toByte
								if ( q != Bottom ) {
									for ( m <- 2 to (iterations - iter) optimized ) {
										terrain(node.v + Cardinals.cardinals(q) * m) = 0.toByte
									}
								}
							}
						}
					}
				}
			}
		}
	}

	/**
	 * Perform this component's generation, storing the results in the provided world.
	 * @return any new generators that should be used from this point on (i.e. a new geological feature, building, etc)
	 */
	def generate(world: World, stage: Stage, phase: Phase, progress: Progress): List[TWorldGenerator] = {
		if ( phase == WorldGenerator.Hydrological ) {
			val heightmap = world.aux[TopologicalData].heightmap
			val depthmap = world.aux[TopologicalData].depthmap
			val reg = world.worldRegionAsSpatialRegion
			if ( stage == WorldGenerator.Heightmap ) {
				val FD = world.aux[GeologicalFeatureData]
				val WorldScale = world.aux[ScaleData]
				val SnowLine = WorldScale.SnowLine.inVoxels.toInt
				val rivers = for ( feature <- FD.allFeatures /* if heightmap(feature.location.xy) > SnowLine */ ) yield {
					var newOrigin : ReadVec2i = feature.location.xy
					val query = GeneralizedSearcher.SearchQuery[ReadVec2i](
						feature.location.xy,
						1000,
						(v,g) => heightmap(v) + g * 0.01f,
						(v) => reg.contains(v),
						(v) => {
							newOrigin = v
							heightmap(v) < SnowLine
						},
						(v,buf) => Cardinals.cardinals.foreach( c => buf.append(v + c.xy) ),
						(from,to) => 1
					)

					GeneralizedSearcher.search(query)
					new River(VoxelCoord(newOrigin.x,newOrigin.y,VoxelCoord.Center.z + heightmap(newOrigin)))
				}

				world.aux[RiverData].rivers = rivers

			} else if ( stage == WorldGenerator.Voxel ) {
				for ( river <- world.aux[RiverData].rivers ) {
					flowRiver(world,river)

					world.aux[FluidData].infiniteFluidSources ::= new InfiniteFluidSource(river.source,Material.materialWithName("water"))
				}

				val computor = new FinitePushFluidComputor
				computor.settleInitialSimulation(world)
			}
		}
		Nil
	}
}


class RiverData extends TWorldAuxData {
	var rivers = List[River]()
}

class River(var source : VoxelCoord) {
	var originalCourse = List[Vec2i]()
}