package arx.axistential.testbed.networking

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/3/15
 * Time: 7:59 AM
 */

import java.util.concurrent.locks.LockSupport

import arx.Prelude._
import arx.axistential.game.archetypes.Material
import arx.axistential.game.archetypes.item.ItemArchetype
import arx.axistential.game.commands.AddColonyGoalCommand
import arx.axistential.game.components.client.ClientSidePhysicsComponent
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.entities.groups.Colony
import arx.axistential.game.entities.traits.PhysicalEntityData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.ai.goals.BuildVoxelGoal
import arx.axistential.game.logic.lighting.LightingComponent
import arx.axistential.game.logic.requirements.SpecificMaterialDescriptor
import arx.axistential.game.networking.communicators.PhysicsNetworkingCommunicator
import arx.axistential.graphics.components.ai.GoalGraphicsComponent
import arx.axistential.graphics.components.entities.PhysicalEntityViewerComponent
import arx.axistential.graphics.components.environmentviewer.EnvironmentViewerComponent2
import arx.core.units.UnitOfTime
import arx.core.units.Velocity
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.voxelregions.VoxelRegion
import arx.tmp.game.logic.world.data.TimeData
import arx.tmp.game.networking.components.NetworkingServerGameComponent
import arx.tmp.game.networking.data.NetworkingWorldData

object NetworkingTestServer {
	def main(args: Array[String]) {
		val server = new ArxServer(1024 * 1024 * 20,1024 * 1024 * 10)
		server.start()
		server.bind(13013,13014)

		val gameEngine = new GameEngine

		try {
			setUpGameEngine(gameEngine)

			gameEngine addComponent new NetworkingServerGameComponent(server)

			gameEngine.initialize()
			gameEngine.start()

			while (true) {
				gameEngine.update(1.0f)
				LockSupport.parkNanos(10000000)
			}
		} finally {
			server.stop()
		}
	}

	def setUpGameEngine (gameEngine : GameEngine): Unit = {
		val ent = ItemArchetype.archetypeWithName("statue").createInstance
		ent.position = ObjectCoord(0.0f,0.0f,0.0f)
		ent.dynamic = true

		gameEngine.world.addEntity(ent)

		val stone = Material.materialWithName("stone")
		val TD = gameEngine.world.aux[TerrainData]
		for (x <- -10 to 10 ; y <- -10 to 10 ; z = -9) {
			TD.setMaterialAt(VoxelCoord.fromRelative(x,y,z),stone)
		}
		TD.materialGrid.enableLogging()

		val ND = gameEngine.world.aux[NetworkingWorldData]
		val physEntities = gameEngine.world.createEntityTypeQuery[TPhysicalEntity]
		ND.customCommunicators += new PhysicsNetworkingCommunicator(physEntities,gameEngine.world)

		gameEngine addComponent new GameEngineComponent {
			var lastUpdate = 0.seconds

			override def update(deltaTime: UnitOfTime): Unit = {
				for (ent <- world.entities if ent.hasAuxData[PhysicalEntityData]) {
					val D = ent.aux[PhysicalEntityData]
					val lastPos = D.position
					D.position = ObjectCoord(D.position.x,D.position.y,cosf(world.aux[TimeData].time.inSeconds) * 10.0f)
					val delta = (D.position - lastPos) / deltaTime.inSeconds
					val vel = Velocity(delta.x.v_s,delta.y.v_s,delta.z.v_s)
					D.velocity = vel
				}


				val TD = world.aux[TerrainData]
				var v = VoxelCoord.fromRelative(rand(-10,10),rand(-10,10),-9)

				while (TD.materialGrid(v) != 0) {
					v = v.plusZ(1)
				}

				val soil = Material.materialWithName("soil")
				val curTime = world.aux[TimeData].time
				if (curTime - lastUpdate > 0.2.seconds) {
					TD.materialGrid.modificationBlock(v) {
						TD.setMaterialAt(v,soil)
					}
					lastUpdate = curTime
				}
			}
		}
	}
}


class NetworkingTestbedModuleConfig extends ModuleConfig {
	override def setUpGameEngine(gameEngine: GameEngine): Unit = {
		val col = new Colony

		val ent = ItemArchetype.archetypeWithName("pile").createInstance
		ent.position = ObjectCoord(0.0f,0.0f,0.0f)
		ent.dynamic = true

		gameEngine.world.addEntity(ent)
		gameEngine.world.addEntity(col)

		val stone = Material.materialWithName("stone")
		val TD = gameEngine.world.aux[TerrainData]
		for (x <- -10 to 10 ; y <- -10 to 10 ; z = -9) {
			TD.setMaterialAt(VoxelCoord.fromRelative(x,y,z),stone)
		}
		TD.materialGrid.enableLogging()

		val ND = gameEngine.world.aux[NetworkingWorldData]
		val physEntities = gameEngine.world.createEntityTypeQuery[TPhysicalEntity]
		ND.customCommunicators += new PhysicsNetworkingCommunicator(physEntities,gameEngine.world)

		gameEngine addComponent new GameEngineComponent {
			var lastUpdate = 0.seconds

			override def update(deltaTime: UnitOfTime): Unit = {
				for (ent <- world.entities if ent.hasAuxData[PhysicalEntityData]) {
					val D = ent.aux[PhysicalEntityData]
					val lastPos = D.position
					D.position = ObjectCoord(D.position.x,D.position.y,cosf(world.aux[TimeData].time.inSeconds) * 10.0f)
					val delta = (D.position - lastPos) / deltaTime.inSeconds
					val vel = Velocity(delta.x.v_s,delta.y.v_s,delta.z.v_s)
					D.velocity = vel
				}


//				val TD = world.aux[TerrainData]
//				var v = VoxelCoord.fromRelative(rand(-10,10),rand(-10,10),-9)
//
//				while (TD.materialGrid(v) != 0) {
//					v = v.plusZ(1)
//				}
//
//				val soil = Material.materialWithName("soil")
//				val curTime = world.aux[TimeData].time
//				if (curTime - lastUpdate > 0.2.seconds) {
//					TD.materialGrid.modificationBlock(v) {
//						TD.setMaterialAt(v,soil)
//					}
//					lastUpdate = curTime
//				}
			}
		}
	}

	override def setUpGraphicsEngine(gameEngine : GameEngine, graphicsEngine: GraphicsEngine): Unit = {
		graphicsEngine addComponent new EnvironmentViewerComponent2
		graphicsEngine addComponent new CylindricalSkyboxGraphicsComponent
		graphicsEngine addComponent new PhysicalEntityViewerComponent
		graphicsEngine addComponent new GoalGraphicsComponent
	}

	override def setUpClientGameEngine(gameEngine: GameEngine): Unit = {
		gameEngine addComponent new ClientSidePhysicsComponent
		gameEngine addComponent new LightingComponent
	}

	override def setUpUI(gameEngine: GameEngine, gameController : GameController): Unit = {
		gameController.addTopLevelMode(new TGameMode {
			onEvent {
				case WorldMousePressEvent(_,start,end,mod) => {
					val col = gameEngine.world.entitiesOfType[Colony].head
					
					val mat = SpecificMaterialDescriptor(Material.withName("iron"))
					val goal = BuildVoxelGoal(VoxelRegion(VoxelCoord.Center,VoxelCoord.Center.plusZ(10)),mat)
					val addGoalCommand = AddColonyGoalCommand(col,goal)
					val ND = world.aux[NetworkingWorldData]
					ND.addCommandToSend(addGoalCommand)
				}
			}
		})
	}
}




