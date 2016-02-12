package arx.axistential.game.world.generation.generators

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/6/13
 * Time: 1:28 PM
 */

import arx.Prelude._
import arx.axistential.game.archetypes.Covering
import arx.axistential.game.archetypes.Material
import arx.axistential.game.data.world._
import arx.axistential.game.world.generation.TWorldGenerator
import arx.axistential.game.world.generation.Progress
import arx.axistential.game.world.generation.WorldGenerator
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.GridCell2D
import arx.tmp.game.logic.datastructures.Talea
import arx.tmp.game.logic.window.GridCellView

import scalaxy.loops._

class RawTerrainGenerator extends TWorldGenerator {
	name = "Full voxel filler"

	@inline
	private final def bcell( cell : GridCellView[Int], x : Int , y : Int ) = cell( math.max(math.min(x,Talea.dimension-1),0) , math.max(math.min(y,Talea.dimension-1),0) )

	def generate ( world : World , stage : WorldGenerator.Stage , phase : WorldGenerator.Phase , progress : Progress ) : List[TWorldGenerator] = {
		if ( stage == WorldGenerator.Voxel && phase == WorldGenerator.Geological ) {
			val TD = world.aux[TerrainData]
			val TopD = world.aux[TopologicalData]
			val WorldScale = world.aux[ScaleData]

			val heightmap = TopD.heightmap
			val depthmap = TopD.depthmap
			val coverings = TD.coveringGrid

			val allBlocks = TD.materialGrid.allInRegionInclusive( world.worldRegionAsSpatialRegion )
			val layerData = world.auxData[LayerData]

			val dirt = TD.materialMapping( Material.materialWithName("soil") )
			val snow = TD.coveringTypes(Covering(Material.materialWithName("snow")))

			allBlocks.parTS(taskSupport).foreach ( talea => {
				val cell = heightmap.cellView(talea.x,talea.y)
				cell.alignTo(talea)
				val depthCell = depthmap.cellView(talea.x,talea.y)
				val layerCell = layerData.layerShiftView(talea.x,talea.y)
				depthCell.alignTo(talea)
				posit(cell.offsetX >= 0 && cell.offsetX <= GridCell2D.dimension - Talea.dimension && cell.offsetY >= 0 && cell.offsetY <= GridCell2D.dimension - Talea.dimension ,
					"grabbed the wrong heightmap cell, offset : " + cell.offsetX + "," + cell.offsetY + " Cell : " + cell + " talea position " + talea.position )


				val taleaZRelativeToWorldCenter = talea.z - world.center.z
				val snowLineInVoxels = WorldScale.SnowLine.inVoxels

				for ( x <- 0 until Talea.dimension optimized ; y <- 0 until Talea.dimension optimized ) {
					val baseHeight = cell(x,y)

					var slopeSum = 0
					slopeSum += math.abs(bcell(cell,x+1,y) - baseHeight)
					slopeSum += math.abs(bcell(cell,x-1,y) - baseHeight)
					slopeSum += math.abs(bcell(cell,x,y+1) - baseHeight)
					slopeSum += math.abs(bcell(cell,x,y-1) - baseHeight)

					val steepness = slopeSum / 4.0f//(math.abs(slopes(0)) + math.abs(slopes(1)) + math.abs(slopes(2)) + math.abs(slopes(3))) / 4.0f

					val zlimit = baseHeight - taleaZRelativeToWorldCenter
					val floor = depthCell(x,y)
					val zmin = floor - taleaZRelativeToWorldCenter
					val boundedZlimit = math.min( zlimit , Talea.dimension )
					for ( z <- math.max(zmin,0) until boundedZlimit optimized ) {
						val layer = layerData.layer(x,y,z + talea.z,layerCell(x,y))
						if ( layer.layerType == GeologicalLayer.Type.Soil || zlimit - z < 8 ) {
							if ( steepness < 4 ) {
								talea.setUnsafeUnlogged(x,y,z,dirt)
								if ( zlimit - z == 1 && steepness < 4.0f && taleaZRelativeToWorldCenter + z > snowLineInVoxels ) {
									for ( q <- 0 until 6 optimized ) {
										coverings(x,y,z,q) = snow
									}
								}
							}
						} else {
							talea.setUnsafeUnlogged(x,y,z,layer.mainMaterialBytes(0))
						}
					}
				}

				if ( ! talea.areAll(0.toByte) ) {
					talea.modifiedCount += 1
				}
			})
		}

		Nil
	}
}
