package arx.axistential.testbed.ai.main

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 3/25/15
 * Time: 7:29 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.ai.AIGameComponent
import arx.axistential.ai.AILogging
import arx.axistential.ai.AISupportComponent
import arx.axistential.game.archetypes.species.AnimalSpecies
import arx.axistential.game.archetypes.species.PlantSpecies
import arx.axistential.game.components.physics.BulletPhysicsComponent
import arx.axistential.game.components.physics.VoxelDebugGraphicsComponent
import arx.axistential.game.components.vegetation.VegetationGameComponent
import arx.axistential.game.data.entity.plants.PlantData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.data.world.TopologicalData
import arx.axistential.game.logic.ai.PathfindingLogging
import arx.axistential.game.logic.ai.passivegoals.FleeFromEnemies
import arx.axistential.game.logic.general.PlantLogic
import arx.axistential.game.logic.general.TerrainLogic
import arx.axistential.game.logic.lighting.LightingComponent
import arx.axistential.game.world.generation.WorldGenerator
import arx.axistential.graphics.camera.TerrainAwareCamera
import arx.axistential.graphics.components.SkyboxComponent
import arx.axistential.graphics.components.ai.GoalGraphicsComponent
import arx.axistential.graphics.components.entities.PhysicalEntityViewerComponent
import arx.axistential.graphics.components.environmentviewer.EnvironmentViewerComponent2
import arx.axistential.modules.hunger.EatWhenHungry
import arx.axistential.modules.hunger.MetabolicLogging
import arx.axistential.testbed.worldgen.TestbedWorldGenerationDescription
import arx.core.vec.coordinates.VoxelCoord
import arx.graphics.GL
import arx.graphics.Image
class AnimalBehaviorTestbed extends BareBonesTestbed {
	def world = gameEngine.world

	def createWorld(): Unit = {
		val TD = world.aux[TerrainData]
		val PD = world.aux[TopologicalData]

		val worldGenerator = new WorldGenerator
		worldGenerator.generateWorld(world,new TestbedWorldGenerationDescription)
		
		val region = world.worldRegion

		var oakLocations = List[VoxelCoord]()
		for (i <- 0 until 10) {
			val testOak = PlantSpecies.withName("oak")
			var location = VoxelCoord.Sentinel
			while (location.isSentinel) {
				val tmp = TerrainLogic.oneAboveFirstTerrainInColumn(rand(region.min.x,region.max.x),rand(region.min.y,region.max.y),TD,PD)
				if (oakLocations.forall(l => l.distanceTo(tmp) > 8.voxels)) {
					location = tmp
				}
			}
			oakLocations ::= location
			val oak = PlantLogic.createPlant(testOak,world,location,None)
			oak.aux[PlantData].born = -2.turning
		}


		val horseSpecies = AnimalSpecies.withName("horse")
		val horse = horseSpecies.createInstance
		val initialFootPos = TerrainLogic.oneAboveFirstTerrainInColumn(VoxelCoord.Center.x,VoxelCoord.Center.y,TD,PD)
		horse.position = initialFootPos.toObjectCoord.plusZ(horse.boundingDimensions.z.inVoxels * 0.5f + 0.01f)
		horse.passiveGoals ::= new EatWhenHungry
		horse.passiveGoals ::= new FleeFromEnemies
		world.addEntity(horse)


		val wolfSpecies = AnimalSpecies.withName("wolf")
		val wolf = wolfSpecies.createInstance
		val wolfFootPos = TerrainLogic.oneAboveFirstTerrainInColumn(VoxelCoord.Center.x + 15,VoxelCoord.Center.y,TD,PD)
		wolf.position = wolfFootPos.toObjectCoord.plusZ(wolf.boundingDimensions.z.inVoxels * 0.5f + 0.01f)
		wolf.passiveGoals ::= new EatWhenHungry
		world.addEntity(wolf)

		AILogging.setLoggingLevel(Noto.Finest)
		MetabolicLogging.loggingLevel = Noto.Finest
		PathfindingLogging.loggingLevel = Noto.Finest
	}

	override def setUpGameEngine(): Unit = {
		createWorld()


		gameEngine addComponent new LightingComponent
		gameEngine addComponent new AIGameComponent
		gameEngine addComponent new AISupportComponent
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
		graphicsEngine addComponent new GoalGraphicsComponent
		graphicsEngine addComponent new VoxelDebugGraphicsComponent
		
//		graphicsEngine addComponent new ShinDynamicGraphicsComponent {
//			lazy val shader = GameUIShader(world,graphicsEngine)
//			val colors = new mutable.HashMap[VoxelCoord,Vec4f]()
//			override def update(dt: UnitOfTime, bucket: RenderBucket): Unit = {
//				val region = world.worldRegion
//				for (ent <- world.entities.ofType[TPhysicalEntity].filter(_.hasAuxData[AnimalAIData]).find(_.name.contains("wolf"))) {
//					val AD = ent.aux[AnimalAIData]
//					val TD = world.terrainData
//					val PD = world.topologicalData
//					for (x <- region.min.x to region.max.x optimized ; y <- region.min.y to region.max.y optimized) {
//						val v = TerrainLogic.oneAboveFirstTerrainInColumn(x,y,TD,PD)
//						AD.patchForIfExists(v) match {
//							case Some(p) =>
//								val color = colors.getOrElseUpdate(p.region.center,Color.rand * Vec4f(1.0f,1.0f,1.0f,0.5f))
//								CommonRendering.quad(bucket,v.toObjectCoord,Vec3f.UnitX,Vec3f.UnitY,Vec2f.One,color,bucket(image("default/blank.png")))
//							case None =>
//						}
//					}
//				}
//			}
//			override def bucketIdentifier: Symbol = 'patchViz
//			override def bucketRequirements = RenderBucketRequirements(UIAttributeProfile,shader)
//		}

		graphicsEngine.postProcessors ::= new DepthOfFieldPostProcessor


		graphicsEngine.pov = new TerrainAwareCamera(gameEngine.world)
		graphicsEngine.pov.viewDistance = 200.0f
		graphicsEngine.pov.asInstanceOf[AnthologiconCamera].useGlobalUp = true
	}
}

