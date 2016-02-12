package arx.axistential.testbed.thermo.application

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/5/13
 * Time: 3:00 PM
 */

import arx.Prelude._
import arx.application.Application
import arx.application.Noto
import arx.axistential.ai.AIGameComponent
import arx.axistential.ai.AILogging
import arx.axistential.game.archetypes.Material
import arx.axistential.game.archetypes.item.ItemArchetype
import arx.axistential.game.archetypes.species.AnimalSpecies
import arx.axistential.game.components._
import arx.axistential.game.components.physics.BulletPhysicsComponent
import arx.axistential.game.components.physics.DebugData
import arx.axistential.game.components.physics.VoxelDebugGraphicsComponent
import arx.axistential.game.components.vegetation.VegetationGameComponent
import arx.axistential.game.data.entity.InventoryData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.data.world.TopologicalData
import arx.axistential.game.entities.CreatureEntity
import arx.axistential.game.entities.groups.Colony
import arx.axistential.game.logic.ai.goals.BuildVoxelGoal
import arx.axistential.game.logic.ai.goals.EmptyInventoryGoal
import arx.axistential.game.logic.ai.passivegoals.FleeFromEnemies
import arx.axistential.game.logic.ai.passivegoals.MoveOutOfBadLocationGoal
import arx.axistential.game.logic.lighting.LightingComponent
import arx.axistential.game.logic.requirements.MaterialTypeDescriptor
import arx.axistential.game.world.generation.generators.botanical.VegetationGenerator
import arx.axistential.game.world.generation.generators.geological.GeologicalFeatureGenerator
import arx.axistential.game.world.generation.generators.RawTerrainGenerator
import arx.axistential.game.world.generation.generators.TopologyGenerator
import arx.axistential.game.world.generation.WorldGenerationDescription
import arx.axistential.game.world.generation.WorldGenerator
import arx.axistential.graphics.components.ai.GoalGraphicsComponent
import arx.axistential.graphics.components.entities.PhysicalEntityViewerComponent
import arx.axistential.graphics.components.environmentviewer.EnvironmentViewerComponent2
import arx.axistential.graphics.components.fluid.FluidViewerComponent2
import arx.axistential.graphics.components.weather.CloudGameEngineComponent
import arx.axistential.graphics.components.SkyboxComponent
import arx.axistential.graphics.components.ui
import arx.axistential.ui.modes.ConstructMode
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.Vec3f
import arx.core.vec.Vec3i
import arx.tmp.game.logic.world.SpatialRegion

class GoalUITestbed extends BareBonesTestbed {
	AILogging.setLoggingLevel(Noto.Fine)
	val colony = new Colony

	def setUpGameEngine(): Unit = {
		val world = gameEngine.activeWorld

		val seed = System.currentTimeMillis()
		val desc = new WorldGenerationDescription(
			List(
				new TopologyGenerator,
				new RawTerrainGenerator,
				new VegetationGenerator,
				new GeologicalFeatureGenerator
//				new RiverGenerator
			),
			Vec3i(768,768,512)
			,seed
		)
		println(s"Seed : $seed")

		val worldgen = new WorldGenerator

		worldgen.generateWorld(world,desc)

		world.addEntity(colony)
		colony.addPassiveGoalCreator(_ => List(new FleeFromEnemies,new EmptyInventoryGoal,new MoveOutOfBadLocationGoal))

		val workers = fillList(3)(i => {
			val w = new CreatureEntity
			w.species = AnimalSpecies.archetypeWithName("human")
			val basePos = VoxelCoord.Center.plusX(i*3)
			val h = world.aux[TopologicalData].heightmap(basePos)
			w.position = basePos.plusZ(h+2).toObjectCoord

			world.addEntity(w)
			w
		})
		workers.foreach( colony.addEntity )

		val pile = ItemArchetype.archetypeWithName("pile").createInstance
		pile.position = ObjectCoord(0f,4f,0)
		val h = world.aux[TopologicalData].heightmap(pile.position.toVoxelCoord)
		pile.position = pile.position.plusZ(h + pile.boundingDimensions.z.inVoxels * 0.5f)

		for ( i <- 0 until 200 ) {
			pile.aux[InventoryData].holdEntityForced( Material.withName("stone").createInstance )
		}

		world.addEntity(pile)
		colony.addEntity(pile)


		gameEngine.addComponent( new LightingComponent )
		gameEngine.addComponent( new CloudGameEngineComponent )
		gameEngine.addComponent( new FluidGameComponent )
		gameEngine.addComponent( new VegetationGameComponent )

		gameEngine.addComponent( new AIGameComponent )
		gameEngine.addComponent( new EnemyAwarenessComponent )
		gameEngine.addComponent( new BulletPhysicsComponent )
	}

	val EVC2 = new EnvironmentViewerComponent2

	def setUpGraphicsEngine(): Unit = {
		gameEngine.world.aux[DebugData].active = true

		DebugData.worldRef = gameEngine.world
		graphicsEngine.addComponent( EVC2 )
		graphicsEngine.addComponent( new PhysicalEntityViewerComponent )
		graphicsEngine.addComponent( new SkyboxComponent )
		graphicsEngine.addComponent( new ui.ModeUIGraphicsComponent(gameController.modeStack) )
		graphicsEngine addComponent new GoalGraphicsComponent
		graphicsEngine.addComponent( new FluidViewerComponent2 )
		graphicsEngine.addComponent( new VoxelDebugGraphicsComponent )

		val camera = new AnthologiconCamera(Vec3f(-60,0.0f,30.0f))
		camera.useGlobalUp = true
		camera.viewDistance = 300.0f
		camera.moveSpeed *= 0.5f
		graphicsEngine.pov = camera

	}

	def setUpUI(): Unit = {
//		gameController.addTopLevelMode(new TestbedGameMode)
		gameController.addTopLevelMode(new ConstructMode)
	}

	class TestbedGameMode extends TGameMode {
		var mode : Int = 0
		var originPoint : Option[VoxelCoord] = None
		def currentVolume = {
			val (start,end) = currentWorldMouseLine
			graphicsEngine.intersectWorld(start,end) match {
				case Some(WorldIntersectionResult(voxel,point,side)) => {
					originPoint match {
						case None => (voxel + dirvec(side),voxel + dirvec(side))
						case Some(origin) => {
							val end = voxel + dirvec(side)
							val min = VoxelCoord(origin.min(end))
							val max = VoxelCoord(origin.max(end))

							min -> max
						}
					}
				}
				case _ => VoxelCoord.Sentinel -> VoxelCoord.Sentinel
			}
		}


		onEvent {
			case kpe : KeyPressEvent => {
				if ( kpe.key == Keyboard.KEY_EQUALS ) { gameEngine.increaseTimescale() }
				else if ( kpe.key == Keyboard.KEY_MINUS ) { gameEngine.decreaseTimescale() }
			}
			case wme : WorldMousePressEvent => {
				val (start,end) = currentWorldMouseLine
				graphicsEngine.intersectWorld(start,end) match {
					case Some(WorldIntersectionResult(voxel,point,side)) => {
						originPoint = originPoint match {
							case Some(origin) => {
								val (min,max) = currentVolume

								val TD = world.aux[TerrainData]
//								TD.materialGrid.modificationBlock(SpatialRegion.fromCorners(min,max)) {
//									for ( v <- min to max ) {
//										TD.setMaterialAt(VoxelCoord(v), Material.withName("stone") )
//									}
//								}

								val allVoxels = SpatialRegion.fromCorners(min,max).containedPoints
								colony.addGoal(BuildVoxelGoal( allVoxels.toList , new MaterialTypeDescriptor(Material.Stone) ) )

								None
							}
							case None => {
								Some(voxel + dirvec(side))
							}
						}
					}
				}
			}
		}


		override def update(f: Float): Unit = {

		}

		override def drawUI(bucket: RenderBucket): Unit = {
			doOnce( () => Noto.info(s"Time until first draw : " + Application.secondsElapsedSinceProgramStart + "s") )
			val (min,max) = currentVolume
//			CommonRendering.drawVolume(bucket,min,max,Vec4f(1.0f,1.0f,1.0f,0.5f))
		}
	}
}