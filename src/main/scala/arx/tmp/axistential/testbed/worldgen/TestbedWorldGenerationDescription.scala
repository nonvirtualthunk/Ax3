package arx.axistential.testbed.worldgen

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 3/28/15
 * Time: 11:31 AM
 */

import arx.axistential.game.archetypes.species.PlantSpecies
import arx.axistential.game.archetypes.Covering
import arx.axistential.game.archetypes.Material
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.data.world.TopologicalData
import arx.axistential.game.logic.general.TerrainLogic
import arx.axistential.game.world.generation.WorldGenerator.Phase
import arx.axistential.game.world.generation.WorldGenerator.Stage
import arx.axistential.game.world.generation.Progress
import arx.axistential.game.world.generation.TWorldGenerator
import arx.axistential.game.world.generation.WorldGenerationDescription
import arx.axistential.game.world.generation.WorldGenerator
import arx.axistential.testbed.worldgen.TestbedWorldGenerationDescription._
import arx.core.gen.ArxGenerators
import arx.core.gen.SimplexNoise
import arx.core.vec.ReadVec3i
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.coordinates.VoxelSideCoord
import arx.engine.world.World

import scalaxy.loops._

class TestbedWorldGenerationDescription extends WorldGenerationDescription(
	List(TerrainGen,PlantGen),ReadVec3i(100,100,100)
)


object TestbedWorldGenerationDescription {
	object TerrainGen extends TWorldGenerator {
		/**
		 * Perform this component's generation, storing the results in the provided world.
		 * @return any new generators that should be used from this point on (i.e. a new geological feature, building, etc)
		 */
		override def generate(world: World, stage: Stage, phase: Phase, progress: Progress): List[TWorldGenerator] = {
			if (stage == WorldGenerator.Heightmap && phase == WorldGenerator.Geological) {
				val TD = world.aux[TerrainData]
				val PD = world.aux[TopologicalData]

				val gen = ArxGenerators.Scale(0.02f,0.02f,0.02f) >> ArxGenerators.Simplex(new SimplexNoise(1337L)) >> ArxGenerators.Mult(5.0f)

				val region = world.worldRegion


				val Soil = Material.withName("soil")
				for (xy <- region.min.xy to region.max.xy) {
					val h = gen(xy)
					PD.heightmap(xy) = h.toInt
					PD.depthmap(xy) = -10

					for (dz <- -10 until h.toInt optimized) {
						TD.setMaterialAt(xy.x,xy.y,VoxelCoord.Center.z + dz,Soil)
					}
				}
			}
			Nil
		}
	}

	object PlantGen extends TWorldGenerator {
		/**
		 * Perform this component's generation, storing the results in the provided world.
		 * @return any new generators that should be used from this point on (i.e. a new geological feature, building, etc)
		 */
		override def generate(world: World, stage: Stage, phase: Phase, progress: Progress): List[TWorldGenerator] = {
			if (stage == WorldGenerator.Voxel && phase == WorldGenerator.Botanical) {
				val TD = world.aux[TerrainData]
				val PD = world.aux[TopologicalData]

				val region = world.worldRegion
				for (x <- region.min.x to region.max.x optimized; y <- region.min.y to region.max.y optimized) {
					val v = TerrainLogic.oneAboveFirstTerrainInColumn(x,y,TD,PD)
					//			TD.setMaterialAt(v,Material.withName("stone"))
					TD.setCoveringAt(VoxelSideCoord(v.minusZ(1),Top),Covering(PlantSpecies.withName("grass")))
				}
			}
			Nil
		}
	}
}