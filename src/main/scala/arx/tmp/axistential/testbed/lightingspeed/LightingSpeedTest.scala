package arx.axistential.testbed.lightingspeed

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/22/14
 * Time: 8:24 AM
 */

import arx.axistential.game.logic.lighting.LightingComponent
import arx.axistential.game.world.generation.generators.geological.GeologicalFeatureGenerator
import arx.axistential.game.world.generation.generators.RawTerrainGenerator
import arx.axistential.game.world.generation.generators.TopologyGenerator
import arx.axistential.game.world.generation.WorldGenerationDescription
import arx.axistential.game.world.generation.WorldGenerator
import arx.core.vec.Vec3i
import arx.engine.game.GameEngine
import arx.engine.world.World

class LightingSpeedTest {
	def run () {
		val gameEngine = new GameEngine
		val world = new World
		gameEngine.environments ::= world

		val desc = new WorldGenerationDescription(
			List(
				new TopologyGenerator,
				new RawTerrainGenerator,
				new GeologicalFeatureGenerator
			),
			Vec3i(768,768,768)
			,1385743720455L
//			,1337L
		)

		val worldgen = new WorldGenerator

		worldgen.generateWorld(world,desc)

		gameEngine.addComponent(new LightingComponent)

		gameEngine.initialize()
		gameEngine.start()
		gameEngine.exit()
	}
}

object LightingSpeedTest {
	def main (args :Array[String]) {
		val lst = new LightingSpeedTest
		lst.run ()
	}
}
