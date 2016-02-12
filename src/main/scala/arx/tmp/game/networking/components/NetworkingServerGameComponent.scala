package arx.tmp.game.networking.components

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/6/15
 * Time: 10:51 AM
 */

import java.util.concurrent.locks.LockSupport

import arx.Prelude._
import arx.application.Noto
import arx.core.datastructures.Killable
import arx.core.datastructures.KillableThread
import arx.core.datastructures.SynchronizedQueue
import arx.core.query.ContinuousQueryListener
import arx.core.units.UnitOfTime
import arx.tmp.game.logic.command.GameCommand
import arx.tmp.game.logic.command.WorldCommand
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.world.data.TNetworkedWorldData
import arx.tmp.game.logic.world.data.TimeData
import arx.tmp.game.networking.data.NetworkingWorldData
import arx.tmp.game.networking.messages._
import arx.tmp.game.networking.NetworkedWorldDataDeltaHandler
import arx.tmp.game.networking.TNetworkedGameEntityAuxData
import com.esotericsoftware.minlog.Log

import scala.collection.mutable

class NetworkingServerGameComponent(server: ArxServer) extends GameEngineComponent with ContinuousQueryListener[GameEntity] {
	lazy val ND = world.aux[NetworkingWorldData]
	lazy val TD = world.aux[TimeData]
	lazy val updateThread = new UpdateThread
	val deltaHandlersByClient = new mutable.HashMap[(Int, Class[_]), NetworkedWorldDataDeltaHandler[_]]()
	val newEntityQueue = new SynchronizedQueue[GameEntity]

	override def initialize(): Unit = {
		world.createEntityTypeQuery[GameEntity].withListener(this,fireOnExistingResults = false)
		server.getKryo.registerClassAndAllSubclasses(classOf[GameEntity],new GameEntitySerializer(gameEngine))

		updateThread.start ()

		server.onEvent {
			case Connected (connection) =>
			case Received (connection, obj) =>
				obj match {
					case MissingEntityMessage (id) => connection.sentEntities.remove (id)
					case wi : WorldInitRequest =>
						val ndata = world.allAuxiliaryData.ofType[TNetworkedWorldData]
						for (d <- ndata) {
							deltaHandlersByClient (connection.getID -> d.getClass) = d.createDeltaHandler

							val initMsg = new WorldDataInitialize
							initMsg.className = d.getClass.getCanonicalName
							initMsg.data = d
							Noto.info (s"Sending ${d.getClass.getSimpleName} aux data init message to client ${connection.getID}")
							connection.sendTCP (initMsg)
						}

						for (ent <- world.entities) {
							sendEntity(ent,connection)
						}

						Log.info(s"Server send of initial world state to client ${connection.getID} completed successfully")
						connection.sendTCP (new WorldInitFinished)
					case GameCommandsMessage(commands) =>
						for (command <- commands) {
							if (command.isValid) {
								command match {
									case gc : GameCommand => gc.performCommand()
									case wc : WorldCommand => wc.performCommand(world)
									case _ => Noto.warn(s"Invalid command type : $command")
								}
							}
						}
					case _ =>
				}
		}
	}

	def sendEntity (ent : GameEntity, conn : ArxConnection): Unit = {
		val networkedAuxData = ent.allAuxiliaryData.ofType[TNetworkedGameEntityAuxData]
		val message = GameEntityInitializeMessage(ent,networkedAuxData)
		conn.sendTCP(message)
		conn.sentEntities.add (ent.uid)
	}

	def sendAuxData(ent: GameEntity, data: TNetworkedGameEntityAuxData) = {
		for (conn <- server.getConnections) {
			val arxConn = conn.asInstanceOf[ArxConnection]

			if (!arxConn.sentEntities.contains (ent.uid)) {
				sendEntity(ent,arxConn)
			}
		}
		val message = new AuxDataUpdateMessage
		message.entityId = ent.uid
		message.auxData = data
		server.sendToAllUDP (message)
	}

	class UpdateThread extends KillableThread (Killable.GameLevel) {
		var lastTimeSync = System.currentTimeMillis()

		override def whileRunningDo(): Unit = {
			var anyChanges = false

			while (newEntityQueue.nonEmpty) {
				newEntityQueue.dequeueOpt() match {
					case Some(ent) =>
						for (connRaw <- server.getConnections) {
							val conn = connRaw.asInstanceOf[ArxConnection]
							if (!conn.sentEntities.contains(ent.uid)) {
								sendEntity(ent,conn)
							}
						}
						anyChanges = true
					case None =>
				}
			}

			while (ND.hasChangedAuxData) {
				for (data <- ND.nextChangedAuxData ()) {
					val ent = data.entity

					sendAuxData(ent,data)
					anyChanges = true
				}
			}

			for (((id, forClass), handler) <- deltaHandlersByClient) {
				for (delta <- handler.extractLatestDeltas) {
					delta.forClass = forClass
					server.sendToTCP (id, delta)
				}
			}

			for (customComm <- ND.customCommunicators) {
				for (msg <- customComm.messagesToSend()) {
					server.sendToAllUDP (msg)
				}
			}

			if (!anyChanges) {
				// 1 millisecond
				LockSupport.parkNanos (1000000l)
			}

			if (System.currentTimeMillis() - lastTimeSync > 5000) {
				server.sendToAllUDP(TimeSyncMessage(TD.time.inSeconds))
				lastTimeSync= System.currentTimeMillis()
			}
		}
	}

	override def update(time: UnitOfTime): Unit = {

	}

	override def queryResultAdded(t: GameEntity): Unit = {
		newEntityQueue.enqueue(t)
	}

	override def queryResultRemoved(t: GameEntity): Unit = {

	}
}
