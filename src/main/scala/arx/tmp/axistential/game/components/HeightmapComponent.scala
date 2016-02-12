package arx.axistential.game.components

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/2/13
 * Time: 11:34 AM
 */

import arx.application.Noto
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.data.world.TopologicalData
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.TaleaGrid.TaleaModificationsCompletedEvent
import arx.tmp.game.logic.datastructures.GridCell2D
import arx.tmp.game.logic.datastructures.TLoggingTalea
import arx.tmp.game.logic.datastructures.TaleaGrid
import com.carrotsearch.hppc.LongLongOpenHashMap

import scala.collection.parallel.ExecutionContextTaskSupport
import scalaxy.loops._

class HeightmapComponent extends GameEngineComponent {
	val lastCheckedMarkers = new LongLongOpenHashMap(100)

	override def initialize(): Unit = {
		val TopData = world.aux[TopologicalData]
		val heightCells = TopData.heightmap.allInRegionInclusiveOrdered( world.worldRegionAsSpatialRegion )
		val depthCells = TopData.depthmap.allInRegionInclusiveOrdered( world.worldRegionAsSpatialRegion )

		timeAndPrint("heightmap normalization") {
			val terrainGrid = world.aux[TerrainData].materialGrid

			heightCells.zip(depthCells).parTS(new ExecutionContextTaskSupport()).foreach {
				case (hcell,dcell) => {
					for ( x <- 0 until GridCell2D.dimension optimized ; y <- 0 until GridCell2D.dimension optimized ) {
						if ( dcell(x,y) > hcell(x,y) ) {
							hcell(x,y) = world.worldRegionAsSpatialRegion.minZ - VoxelCoord.Center.z
						} else {
							var h = hcell(x,y) - 1
							while ( VoxelCoord.Center.z + h > world.worldRegionAsSpatialRegion.minZ &&
										terrainGrid(hcell.x + x,hcell.y + y,VoxelCoord.Center.z + h) == 0 )
							{
								h -= 1
							}
							hcell(x,y) = h + 1
						}
					}
				}
			}
		}

		world.aux[TerrainData].materialGrid.onEvent {
			case TaleaModificationsCompletedEvent(taleae) => {
				val heightmap = world.aux[TopologicalData].heightmap
				val terrainGrid = world.aux[TerrainData].materialGrid
				for ( talea <- taleae ) {
					talea match {
						case ltalea : TLoggingTalea[Byte] => {
							val key = VoxelCoord.hashL(talea.x,talea.y,talea.z)
							val lastChecked = lastCheckedMarkers.containsKey(key) match {
								case false => -1L
								case true => lastCheckedMarkers.lget
							}

							val blockRev = talea.modifiedCount
							if ( lastChecked < blockRev) {
								for ( mod <- ltalea.loggedModifications.takeWhile( _.revision > lastChecked ) ) {
									val ax = talea.x + mod.x
									val ay = talea.y + mod.y
									if ( mod.oldValue == 0 && mod.newValue != 0 ) {
										if ( mod.z + talea.z >= heightmap(ax,ay) + VoxelCoord.Center.z ) {
											heightmap(ax,ay) = mod.z + talea.z - VoxelCoord.Center.z + 1
										}
									} else if ( mod.oldValue != 0 && mod.newValue == 0 ) {
										if ( mod.z + talea.z + 1 == heightmap(ax,ay) + VoxelCoord.Center.z ) {
											var tz = mod.z + talea.z - 1
											while ( tz > world.worldRegionAsSpatialRegion.minZ && terrainGrid(ax,ay,tz) == 0 ) { tz -= 1 }
											heightmap(ax,ay) = tz - VoxelCoord.Center.z + 1
										}
									}
								}

								lastCheckedMarkers.put(key,blockRev)
							}
						}
						case _ => Noto.warn("Heightmap component only works with logging taleae")
					}

				}
			}
		}
	}

	def update(time: UnitOfTime): Unit = {}
}
