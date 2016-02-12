package arx.axistential.testbed.graphics

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/5/15
 * Time: 8:53 AM
 */

import arx.axistential.game.components.physics.BulletPhysicsComponent
import arx.axistential.game.components.vegetation.VegetationGameComponent
import arx.axistential.game.components.FluidGameComponent
import arx.axistential.game.components.WeatherComponent
import arx.axistential.game.logic.lighting.LightingComponent
import arx.axistential.game.world.generation.generators.botanical.VegetationGenerator
import arx.axistential.game.world.generation.generators.geological.GeologicalFeatureGenerator
import arx.axistential.game.world.generation.generators.hydrological.RiverGenerator
import arx.axistential.game.world.generation.generators.meteorological.WindWorldGenerator
import arx.axistential.game.world.generation.generators.RawTerrainGenerator
import arx.axistential.game.world.generation.generators.TopologyGenerator
import arx.axistential.game.world.generation.WorldGenerationDescription
import arx.axistential.game.world.generation.WorldGenerator
import arx.axistential.graphics.components.SkyboxComponent
import arx.axistential.graphics.components.entities.PhysicalEntityViewerComponent
import arx.axistential.graphics.components.environmentviewer.EnvironmentViewerComponent2
import arx.axistential.graphics.components.fluid.FluidViewerComponent2
import arx.axistential.graphics.components.granular.GranularViewerComponent
import arx.axistential.graphics.components.weather.CloudGameEngineComponent
import arx.axistential.graphics.components.weather.WeatherGraphicsComponent
import arx.axistential.testbed.thermo.logic.ThermodynamicsGameComponent
import arx.core.vec.Vec3i
import arx.graphics.GL
import arx.graphics.Image

class DepthOfFieldTestbed extends BareBonesTestbed {
	def world = gameEngine.world

	def createWorld(): Unit = {
		val seed = System.currentTimeMillis()
		val desc = new WorldGenerationDescription(
			List(
				new TopologyGenerator,
				new RawTerrainGenerator,
				new GeologicalFeatureGenerator,
				new RiverGenerator,
				new VegetationGenerator,
				new WindWorldGenerator
			),
			Vec3i(700,700,768)
			,seed
		)
		println(s"Seed : $seed")

		val worldgen = new WorldGenerator

		worldgen.generateWorld(world,desc)
	}

	override def setUpGameEngine(): Unit = {
		createWorld()


		gameEngine addComponent new LightingComponent
		gameEngine addComponent new FluidGameComponent
		gameEngine addComponent new CloudGameEngineComponent
		gameEngine addComponent new ThermodynamicsGameComponent
		gameEngine addComponent new WeatherComponent
		gameEngine addComponent new VegetationGameComponent
		gameEngine addComponent new BulletPhysicsComponent
	}

	override def setUpUI(): Unit = {
		gameController.addTopLevelMode(new TGameMode {
			onEvent {
				case KeyPressEvent(k,_) => k match {
					case Keyboard.KEY_F => {
						println("Attempting to capture depth buffer")
						val img = graphicsEngine.postProcessors.head.fbo.fetchDepthBuffer
						Image.save(img,"/tmp/depthBuffer.png")

						println("Attempting to capture color buffer levels")
						for (l <- 0 until 3) {
							val c = GL.fetchColorTextureData(graphicsEngine.postProcessors.head.fbo.colorID,l)
							Image.save(c,s"/tmp/colorBuffer_$l.png")
						}
					}
					case _ =>
				}
			}
		})
	}

	override def setUpGraphicsEngine(): Unit = {
		graphicsEngine addComponent new EnvironmentViewerComponent2
		graphicsEngine addComponent new SkyboxComponent
		graphicsEngine addComponent new PhysicalEntityViewerComponent
		graphicsEngine addComponent new FluidViewerComponent2
		graphicsEngine addComponent new GranularViewerComponent
		graphicsEngine addComponent new WeatherGraphicsComponent

		graphicsEngine.postProcessors ::= new DepthOfFieldPostProcessor

		graphicsEngine.pov.viewDistance = 200.0f
		graphicsEngine.pov.asInstanceOf[AnthologiconCamera].useGlobalUp = true
	}
}
