package arx.axistential.testbed.worldgen.main

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/6/13
 * Time: 4:18 PM
 */

import arx.axistential.game.components.FluidGameComponent
import arx.axistential.game.logic.lighting.LightingComponent
import arx.axistential.game.world.generation.generators.botanical.VegetationGenerator
import arx.axistential.game.world.generation.generators.geological.GeologicalFeatureGenerator
import arx.axistential.game.world.generation.generators.hydrological.RiverGenerator
import arx.axistential.game.world.generation.generators.RawTerrainGenerator
import arx.axistential.game.world.generation.generators.TopologyGenerator
import arx.axistential.game.world.generation.WorldGenerationDescription
import arx.axistential.game.world.generation.WorldGenerator
import arx.axistential.graphics.components.SkyboxComponent
import arx.axistential.graphics.components.entities.PhysicalEntityViewerComponent
import arx.axistential.graphics.components.environmentviewer.EnvironmentViewerComponent2
import arx.axistential.graphics.components.fluid.FluidViewerComponent2
import arx.axistential.graphics.components.weather.CloudGameEngineComponent
import arx.core.vec.Vec3f
import arx.core.vec.Vec3i
import arx.engine.world.World

class WorldGenBBT extends BareBonesTestbed {
	var fluidComp = new FluidGameComponent

	def setUpGameEngine(): Unit = {
		val world = gameEngine.activeWorld

		val seed = System.currentTimeMillis()
		val desc = new WorldGenerationDescription(
			List(
				new TopologyGenerator,
				new RawTerrainGenerator,
				new GeologicalFeatureGenerator,
				new RiverGenerator,
				new VegetationGenerator
			),
			Vec3i(768,768,768)
		,1385743720455L
//			,seed
		)
		println(s"Seed : $seed")
		
		val worldgen = new WorldGenerator

		worldgen.generateWorld(world,desc)

		
		gameEngine.addComponent( new LightingComponent )
		gameEngine.addComponent( fluidComp )
		gameEngine.addComponent( new CloudGameEngineComponent )
//		fluidComp.isActive = false
	}

	def setUpGraphicsEngine(): Unit = {
		graphicsEngine.addComponent( new EnvironmentViewerComponent2 )
		graphicsEngine.addComponent( new PhysicalEntityViewerComponent )
		graphicsEngine.addComponent( new SkyboxComponent )
		graphicsEngine.addComponent( new FluidViewerComponent2 )
		graphicsEngine.addComponent( new ModeUIGraphicsComponent(gameController.modeStack,"axistential") )

		val camera = new AnthologiconCamera(Vec3f(-60,0.0f,5.0f))
		camera.useGlobalUp = true
		camera.viewDistance = 400.0f
		graphicsEngine.pov = camera

	}

	def setUpUI(): Unit = {
		gameController.addTopLevelMode(new TestbedGameMode)
		//		windowingSystem.giveFocusTo(console.inputWidget)
	}
	
	class TestbedGameMode extends TGameMode {
		onEvent {
			case kpe : KeyPressEvent => {
				if ( kpe.key == Keyboard.KEY_SPACE ) {
					fluidComp.isActive = true
				}
			}
		}
	}
}


object WorldGenBBT {
	def main ( args : Array[String] ) {
		val world = new World
		val desc = new WorldGenerationDescription(
			List(new TopologyGenerator,new RawTerrainGenerator),
			Vec3i(1025,1025,512)
		)

		val worldgen = new WorldGenerator

		worldgen.generateWorld(world,desc)
	}
}