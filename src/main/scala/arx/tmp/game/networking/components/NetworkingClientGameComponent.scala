package arx.tmp.game.networking.components

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/6/15
 * Time: 10:51 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.units.UnitOfTime
import arx.tmp.game.logic.command.TCommand
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.world.data.NetworkedWorldDataUpdate
import arx.tmp.game.logic.world.data.TNetworkedWorldData
import arx.tmp.game.logic.world.data.TimeData
import arx.tmp.game.networking.NetworkedWorldDataDeltaHandler
import arx.tmp.game.networking.data.NetworkingWorldData
import arx.tmp.game.networking.messages._
import com.esotericsoftware.minlog.Log

import scala.collection.mutable

class NetworkingClientGameComponent(client: ArxClient) extends GameEngineComponent {
	val deltaHandlers = new mutable.HashMap[Class[_], NetworkedWorldDataDeltaHandler[_]]()

	var queuedUpdates : List[NetworkedWorldDataUpdate] = Nil

	def processUpdate(wdu: NetworkedWorldDataUpdate) = {
		world._auxData.get (wdu.forClass) match {
			case Some (ad) =>
				ad match {
					case nad: TNetworkedWorldData => {
						val handler = deltaHandlers.getOrElseUpdate (ad.getClass, nad.createDeltaHandler)
						handler.applyLatestDelta (wdu)
					}
					case _ => Noto.error (s"Aux data was non networked?")
				}
			case None => Noto.error (s"No aux data corresponds to received update : $wdu")
		}
	}




	/**
	 * This is something of a hack at the moment. The gist of it is: we want to receive a fully reconstructed
	 * world before we actually do game engine initialization. This allows things like lighting to be correctly
	 * computed without any additional logic. But, for that to happen, we need the networking game component
	 * to be set up and listening before initializaiton, but after the game engine is set. So, here we start
	 * our connection and listeners as soon as we receive a game engine.
	 */
	override def gameEngine_= (ge : GameEngine) {
		if (ge != gameEngine) {
			super.gameEngine_=(ge)

			if (! gameEngine.isClient) {
				throw new IllegalStateException("NetworkingClientGameComponent can only be used with a GameEngine for which isClient is true")
			}

			client.getKryo.registerClassAndAllSubclasses(classOf[GameEntity],new GameEntitySerializer(gameEngine))
			val ND = world.aux[NetworkingWorldData]
			ND.isClient = true

			client.onEvent {
				case Received (connection, obj) =>
					obj match {
						case GameEntityInitializeMessage(entity,auxData) =>
							for (ad <- auxData) {
								entity.manualAddAuxData(ad)
							}
							world.addEntity(entity)
						case adu: AuxDataUpdateMessage =>
							val entityId = adu.entityId
							val auxData = adu.auxData

							val ent = world.entitiesByUID.get (entityId)
							if (ent == null) {
								Noto.error (s"Received an aux data update for nonexistent entity $entityId")
								client.sendTCP (MissingEntityMessage (entityId))
							} else {
								ent.manualAddAuxData(auxData)
							}
						case wdi: WorldDataInitialize =>
							Noto.info (s"Received world data initialization")
							val data = wdi.data
							world.removeAuxData (data.getClass)
							world.manualAddAuxData (data)
						case wdu: NetworkedWorldDataUpdate =>
							if (initialized) {
								processUpdate(wdu)
							} else {
								queuedUpdates :+= wdu
							}
						case wif : WorldInitFinished =>
							Log.info("client","World initialization from server completed")
						case TimeSyncMessage(timeInSeconds) =>
							world.aux[TimeData].time = timeInSeconds.seconds
						case other =>
							var handled = false
							for (comm <- ND.customCommunicators) {
								if (! handled && comm.messageReceived(obj)) {
									handled = true
								}
							}
							// we only want to warn if the whole game engine has actually started up
							if (! handled && initialized) {
								if (other.getClass.getCanonicalName.startsWith ("arx")) {
									Noto.warn ("Unhandled arx message : " + other)
								}
							}
					}
				case Disconnected(conn) =>
					Log.info("networking","Client disconnected from game server")
			}
		}
	}

	override def initialize(): Unit = {

	}

	override def update(time: UnitOfTime): Unit = {
		for (wdu <- queuedUpdates) {
			processUpdate(wdu)
		}
		queuedUpdates = Nil

		val ND = world.aux[NetworkingWorldData]

		ND.allCommandsToSend match {
			case v : Seq[TCommand] if v.nonEmpty => client.sendTCP(GameCommandsMessage(v))
			case _ =>
		}
	}
}
