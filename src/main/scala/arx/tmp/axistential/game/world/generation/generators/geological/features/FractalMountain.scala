package arx.axistential.game.world.generation.generators.geological.features

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/7/13
 * Time: 9:03 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.data.world.GeologicalFeatureData
import arx.axistential.game.data.world.ScaleData
import arx.axistential.game.data.world.TopologicalData
import arx.axistential.game.world.generation.TWorldGenerator
import arx.axistential.game.world.generation.WorldGenerator.Phase
import arx.axistential.game.world.generation.WorldGenerator.Stage
import arx.axistential.game.world.generation.generators.geological.GeologicalFeature
import arx.axistential.game.world.generation.Progress
import arx.axistential.game.world.generation.WorldGenerator
import arx.core.gen.SimplexSource2D
import arx.core.vec.Vec3i
import arx.tmp.game.logic.datastructures.CellGrid2D
import arx.tmp.game.logic.world.SpatialRegion

class FractalMountain(inWorld : World) extends GeologicalFeature(inWorld) with TWorldGenerator {
	name = "Fractal mountain generator"

	val worldScale = inWorld.aux[ScaleData]
	def peakHeight: Int = ND (randomGenerator, worldScale.MountainPeak.inVoxels, 10.meters.inVoxels).toInt

	extents = Vec3i ((worldScale.MountainPeak.inVoxels * 1.2).toInt, (worldScale.MountainPeak.inVoxels * 1.2).toInt, worldScale.MountainPeak.inVoxels.toInt)

	override def canBePlacedAt(v: Vec3i, env: World, gfd: GeologicalFeatureData): Boolean = {
		super.canBePlacedAt (v, env, gfd)
	}

	override def placeAt(v: Vec3i, env: World, gfd: GeologicalFeatureData) {
		super.placeAt (v, env, gfd)
	}

	val intermediaryCenter = Vec3i (2048, 2048, 0)


	/**
	 * Perform this component's generation, storing the results in the provided world.
 *
	 * @return any new generators that should be used from this point on (i.e. a new geological feature, building, etc)
	 */
	def generate(world: World, stage: Stage, phase: Phase, progress: Progress): List[TWorldGenerator] = {
		if (phase == WorldGenerator.Geological) {
			if (stage == WorldGenerator.Heightmap) {
				val heightmap = world.aux[TopologicalData].heightmap
				val depthmap = world.aux[TopologicalData].depthmap

				val hmap = CellGrid2D[Int]()
				val h = 256
				val h2 = h / 2
				val bottomCorner = intermediaryCenter - h2
				val diamondSquareField = SpatialRegion.fromCorners (bottomCorner, bottomCorner + h)
				diamondSquare (world, hmap, diamondSquareField, h)

				for (x <- -h2 to h2; y <- -h2 to h2) {
					val hmapValue = hmap (diamondSquareField.center.x + x, diamondSquareField.center.y + y)
					heightmap (location.x + x, location.y + y) += hmapValue
					depthmap (location.x + x, location.y + y) -= hmapValue / 10
				}
			}
		}
		Nil
	}


	def diamondSquare(world: World, hmap: CellGrid2D[Int], diamondSquareField: SpatialRegion, h: Int) {
		val heightmap = world.aux[TopologicalData].heightmap
		val basePoint = heightmap (this.location.xy)
		val effPeakHeight = (basePoint + peakHeight).clamp (0, world.halfDimensions.z - 16).toInt
		val randGen = randomGenerator

		val noiseGenerator = simplexGenerator

		val offsetGen = new SimplexSource2D (noiseGenerator).scaleInputs (0.02f, 0.02f)

		var iterationCount = 0
		var iterationIncr = h
		var offsetMag = 200.0

		val peakPoint = diamondSquareField.lowerCorner.xy + (iterationIncr >> 1)
		hmap (peakPoint) = effPeakHeight

		def offset(x: Int, y: Int): Int = {
			val f = randGen.nextFloat () * 2.0f - 1.0f
			//                     val f = offsetGen(x,y)
			(offsetMag * f).toInt
		}

		while (iterationIncr > 0) {
			val halfIterationIncr = iterationIncr >> 1
			//DIAMOND
			for (x <- diamondSquareField.minX + halfIterationIncr to diamondSquareField.maxX by iterationIncr ;
				  y <- diamondSquareField.minY + halfIterationIncr to diamondSquareField.maxY by iterationIncr ) {
				if (hmap (x, y) == 0) {
					val newHeight = ((hmap (x - halfIterationIncr, y - halfIterationIncr) + hmap (x + halfIterationIncr, y + halfIterationIncr) +
						hmap (x - halfIterationIncr, y + halfIterationIncr) + hmap (x + halfIterationIncr, y - halfIterationIncr)) >> 2) +
						offset (x, y)
					hmap (x, y) = newHeight
				}
			}
			//SQUARE
			for (x <- (diamondSquareField.minX + halfIterationIncr) until diamondSquareField.maxX by iterationIncr ;
				  y <- diamondSquareField.minY to diamondSquareField.maxY by iterationIncr ) {
				if (hmap (x, y) == 0) {
					val newHeight =
						if (iterationCount == 0) {
							0
						}
						else {
							((hmap (x - halfIterationIncr, y) + hmap (x, y - halfIterationIncr) + hmap (x + halfIterationIncr, y) + hmap (x, y + halfIterationIncr)) >> 2) + offset (x, y);
						}
					hmap (x, y) = newHeight
				}
			}

			for (x <- diamondSquareField.minX to diamondSquareField.maxX by iterationIncr ;
				  y <- (diamondSquareField.minY + halfIterationIncr) until diamondSquareField.maxY by iterationIncr ) {
				if (hmap (x, y) == 0) {
					val newHeight =
						if (iterationCount == 0) {
							0
						}
						else {
							((hmap (x - halfIterationIncr, y) + hmap (x, y - halfIterationIncr) + hmap (x + halfIterationIncr, y) + hmap (x, y + halfIterationIncr)) >> 2) + offset (x, y);
						}
					hmap (x, y) = newHeight
				}
			}


			offsetMag *= 0.5
			iterationCount += 1
			iterationIncr = halfIterationIncr
		}

		val maxDist = (h / 2) * (h / 2)
		Noto.debug ("min : " + diamondSquareField.lowerCorner + " max : " + diamondSquareField.upperCorner + " center : " + diamondSquareField.center + " h : " + h)
		for (x <- diamondSquareField.minX to diamondSquareField.maxX ; y <- diamondSquareField.minY to diamondSquareField.maxY ) {
			val edist = (x - diamondSquareField.center.x) * (x - diamondSquareField.center.x) + (y - diamondSquareField.center.y) * (y - diamondSquareField.center.y)
			val fdist = 1.0f - ((edist.toFloat / maxDist.toFloat) + offsetGen (x, y) * 0.2f).clamp (0.0f, 1.0f)

			hmap (x, y) = (hmap (x, y) * fdist * fdist).toInt

		}
	}
}
