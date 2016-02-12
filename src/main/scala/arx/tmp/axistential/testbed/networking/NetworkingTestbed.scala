package arx.axistential.testbed.networking

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/16/15
 * Time: 1:21 PM
 */


import arx.axistential.game.components.client.ClientSidePhysicsComponent
import arx.axistential.game.entities.traits.PhysicalEntityData
import arx.axistential.game.logic.lighting.LightingComponent
import arx.axistential.graphics.components.entities.PhysicalEntityViewerComponent
import arx.axistential.graphics.components.environmentviewer.EnvironmentViewerComponent2
import arx.axistential.graphics.shader.GameUIShader
import arx.core.representation.InformationLevel.InformationLevel
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.Vec3f
import arx.core.vec.Vec4f
import arx.tmp.game.networking._
import arx.tmp.game.networking.components.NetworkingClientGameComponent
import arx.tmp.game.networking.messages.WorldInitFinished
import arx.macros.NetworkedAuxData

class NetworkingTestbed extends BareBonesTestbed {
	val networked = true

	var setupCompleted = false
	var worldReceived = false

	override def setUpGameEngine(): Unit = {
		if (networked) {
			val client = new ArxClient(1024 * 1024 * 20, 1024 * 1024 * 10)
			client.start()

			client.onEvent {
				case Received(connection, obj) => obj match {
					case wdi: WorldInitFinished =>
						if (setupCompleted) {
							gameEngine.initialize()
							gameEngine.start()

							graphicsEngine.start()
						}
						worldReceived = true
					case _ => // do nothing
				}
			}

			gameEngine.isClient = true
			gameEngine addComponent new ClientSidePhysicsComponent
			gameEngine addComponent new NetworkingClientGameComponent(client)
		} else {
			NetworkingTestServer.setUpGameEngine(gameEngine)
		}

		gameEngine addComponent new LightingComponent
	}


	override def start(): Unit = {
		setUpGameEngine()
		setUpGraphicsEngine()
		setUpUI()

		SettingsManager.initialize(gameEngine, graphicsEngine)

		gameController.initializeModes()

		if (worldReceived) {
			gameEngine.initialize()
			gameEngine.start()

			graphicsEngine.start()
		}
		setupCompleted = true
	}

	override def setUpUI(): Unit = {}


	var nMinus1Position: ObjectCoord = ObjectCoord.Zero
	var nMinus2Position: ObjectCoord = ObjectCoord.Zero


	override def setUpGraphicsEngine(): Unit = {
		val gc = new ShinDynamicGraphicsComponent {
			lazy val shader = GameUIShader(world, graphicsEngine)

			override def update(dt: UnitOfTime, bucket: RenderBucket): Unit = {
				val texCoords = bucket(image("default/blankBordered.png"))
				for (ent <- world.entities if ent.hasAuxData[PhysicalEntityData]) {
					val rawPos = ent.aux[PhysicalEntityData].position
					val interPos = MathPrelude.cosInterpolatev3(2.0f, List(0.0f -> nMinus2Position, 1.0f -> nMinus1Position))
					val pos = (rawPos + interPos) * 0.5f

					nMinus2Position = nMinus1Position
					nMinus1Position = pos

					CommonRendering.drawCube(bucket, pos, Vec3f.One, Vec4f.One, texCoords)
				}
			}

			override def bucketRequirements = RenderBucketRequirements(UIAttributeProfile, shader)
		}

		graphicsEngine addComponent gc
		graphicsEngine addComponent new EnvironmentViewerComponent2
		graphicsEngine addComponent new CylindricalSkyboxGraphicsComponent
		graphicsEngine addComponent new PhysicalEntityViewerComponent
	}
}


@NetworkedAuxData
class SimplePhysData extends TNetworkedGameEntityAuxData {
	var position = ObjectCoord(0.0f, 0.0f, 0.0f)

	override def informationPairs(level: InformationLevel): Map[String, Any] = Map()
}