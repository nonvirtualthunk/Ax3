package arx.axistential.game.world.generation.generators

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/6/13
 * Time: 1:26 PM
 */

import arx.Prelude._
import arx.axistential.game.data.world.TopologicalData
import arx.axistential.game.world.generation.Progress
import arx.axistential.game.world.generation.TWorldGenerator
import arx.axistential.game.world.generation.WorldGenerator
import arx.core.gen._
import arx.core.vec.Vec2i
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.GridCell2D

import scalaxy.loops._

class TopologyGenerator extends TWorldGenerator {
	name = "Anthologicon basic heightmap generator"

	def generate ( world : World , stage : WorldGenerator.Stage , phase : WorldGenerator.Phase , progress : Progress ) : List[TWorldGenerator] = {
		if ( phase == WorldGenerator.Geological && stage == WorldGenerator.Heightmap ) {
			val heightmap = world.aux[TopologicalData].heightmap
			val depthmap = world.aux[TopologicalData].depthmap

			val heightCells = heightmap.allInRegionInclusiveOrdered(world.worldRegionAsSpatialRegion)
			val depthCells = depthmap.allInRegionInclusiveOrdered(world.worldRegionAsSpatialRegion)
			val allCells = heightCells zip depthCells

			val toComplete = allCells.size.toFloat
			var completed = 0.0f

			allCells.parTS(taskSupport).foreach {
				case (heightCell, depthCell) => {
					val max = 30
					val offset = 0
					val maxFract = max.toFloat / world.halfDimensions.z.toFloat

					import ArxGenerators._
					val turbulence = Translate(heightCell.x,heightCell.y,0.0f) >> Scale(0.006f) >> Translate(-543.0f,-678.0f,0.0f) >> Fractal(2)(Simplex(simplexGenerator)) >> Mult(80.0f)

					val generator = Translate(heightCell.x,heightCell.y,0) >> Scale(0.001f) >> Fractal(4)(Simplex(simplexGenerator)) >> Mult(maxFract)

					val edgeGenerator = Translate(124.5f + heightCell.x, 1741.1f + heightCell.y, 0.0f) >> Scale(0.005f) >> Fractal(4)(Simplex(simplexGenerator) >> Abs)

					val absMax = euclidDistance(world.dimensions.x / 2, 0, Vec2i(0, 0)).toFloat - 8.0f
					val maxDist = absMax * 1.1f
					val oo_maxDist = 1.0f / maxDist

					val expectedFractalRange = 1.6f
					val o_fr = 1.0f / expectedFractalRange

					val avgDepth = -100
					val variancePcnt = 0.35f
					//we want it at the far end to be 0 at the core, and probably 150ish at the low end at the edge
					val variance = ((-avgDepth * 1.75) / variancePcnt).toInt

					val centerxy = world.center.xy
					for ( x <- 0 until GridCell2D.dimension optimized ; y <- 0 until GridCell2D.dimension optimized ) {
						val tx = turbulence(x,y)
						val ty = turbulence(x + 1337.0f,y - 1337.0f)

						val edist = euclidDistance(x + heightCell.position.x, y + heightCell.position.y, centerxy)
						val fdist = edist * oo_maxDist
						val mdist = (fdist * fdist) + (fdist + 0.2f) * (edgeGenerator(x + tx,y + ty) * o_fr - 0.5f) * variancePcnt
						//midst range [-0.35,1.04], at edge, [0.34,1.04], at center [-0.35,0.35]

						val f = generator(x + tx, y + ty)
						val h = (f * world.halfDimensions.z).toInt
						heightCell(x, y) = h - offset

						val depth = avgDepth + mdist * variance
						depthCell(x, y) = depth.toInt - offset
					}
					completed += 1.0f
					progress.percentDone = completed / toComplete
				}
			}
		}

		Nil
	}
}
