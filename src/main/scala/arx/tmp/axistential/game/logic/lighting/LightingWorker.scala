package arx.axistential.game.logic.lighting

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 9/12/15
 * Time: 11:43 AM
 */

import java.util.concurrent.locks.LockSupport

import arx.application.Noto
import arx.axistential.game.logic.lighting.LightingWorker._
import arx.axistential.game.logic.lighting.computors.TGlobalLightComputor
import arx.axistential.game.logic.lighting.computors.TLocalLightComputor
import arx.core.datastructures.Killable
import arx.core.datastructures.KillableThread
import arx.core.datastructures.SynchronizedQueue
import arx.core.vec.Vec3i
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.ITalea
import arx.tmp.game.logic.datastructures.TaleaGrid.TaleaModificationsCompletedEvent
import arx.tmp.game.logic.entities.LightPositionChanged
import arx.tmp.game.logic.entities.LightStrengthChanged
import arx.tmp.game.logic.entities.TLightSource
import arx.tmp.game.logic.event.GameEventData
import arx.tmp.game.logic.lighting._
import arx.tmp.game.logic.world.data.LightData


class LightingThreadWorker(world: World, globalComputor: TGlobalLightComputor, localComputors: List[TLocalLightComputor], queue : SynchronizedQueue[Any]) extends KillableThread(Killable.GameLevel) {
	val subWorker = new LightingWorker(world,globalComputor,localComputors,(m) => queue.enqueue(m))
	override def whileRunningDo(): Unit = {
		queue.dequeueOpt() match {
			case Some(m) => subWorker.processWork(m)
			case _ => LockSupport.parkNanos((0.016667 * 1e9).toLong)
		}
	}
}

class LightingWorker(world: World, globalComputor: TGlobalLightComputor, localComputors: List[TLocalLightComputor], messageQueuer: (Any) => Unit) {
	def processWork(work: Any): Boolean = {
		work match {
			case BulkUpdateLightingTask(terrainTaleae) =>
				val lightData = world.aux[LightData]
				val globalGrid = lightData.globalLighting(0)
				val localChannels = lightData.allLocalLightChannels
				var mods = Set[ITalea[_]]()

				for (terrainTalea <- terrainTaleae) {
					globalGrid.rawGetTaleaIfExists(terrainTalea.position) match {
						case Some(lightTalea) => {
							val lastTriggered = lightTalea.lastTriggered
							val blockRev = terrainTalea.modifiedCount

							if (blockRev > lastTriggered) {
								lightTalea.lastTriggered = blockRev

								mods ++= globalComputor.updateLightingForTalea(world, globalGrid, lightTalea, primaryQ, gli)
							}
						}
						case _ =>
					}

					for (channel <- localChannels) {
						channel.lightSources match {
							case headLightSource :: _ => {
								channel.grid.rawGetTaleaIfExists(terrainTalea.position) match {
									case Some(lightTalea) => {
										val lastTriggered = lightTalea.lastTriggered
										val blockRev = terrainTalea.modifiedCount

										if (blockRev > lastTriggered) {
											lightTalea.lastTriggered = blockRev

											localComputors.find(_.canHandleLightSource(headLightSource)) match {
												case Some(computor) => mods ++= computor.updateLightingForTalea(world, channel, lightTalea)
												case None => Noto.warn("No computor could be found to handle light source : " + headLightSource + " " + headLightSource.getClass.getSimpleName)
											}
										}
									}
									case _ =>
								}
							}
							case _ =>
						}
					}
				}
				if (mods.isEmpty && !terrainTaleae.isEmpty) {
					//						Noto.warn("Modification block did not result in any lighting changes, originating changes: " + terrainTaleae.map(_.position.toString).reduce(_ + ","+ _))
					globalGrid.fireEvent(TaleaModificationsCompletedEvent(terrainTaleae))
				} else {
					globalGrid.fireEvent(TaleaModificationsCompletedEvent(mods))
				}
			case AddNewLightSourceTask(lightSource) =>
				addNewLightSource(lightSource)
			case MoveLightSourceTask(lightSource, oldPos, newPos) => {
				//check to ensure it was already calculated at oldPos before moving it
				Noto.finest("moving light source " + lightSource + " from " + oldPos + " to " + newPos)
				localComputors.find(_.canHandleLightSource(lightSource)) match {
					case Some(computor) =>
						val delta = newPos - oldPos
						var pos = oldPos
						var i = 0
						while (i < 3) {
							var di = delta(i)
							while (di != 0) {
								if (di > 0) {
									val minidelta = Vec3i(0, 0, 0)
									minidelta(i) = 1
									computor.moveLightSource(world, lightSource, pos, pos + minidelta)
									pos = pos + minidelta
									di -= 1
								}
								else if (di < 0) {
									val minidelta = Vec3i(0, 0, 0)
									minidelta(i) = -1
									computor.moveLightSource(world, lightSource, pos, pos + minidelta)
									pos = pos + minidelta
									di += 1
								}
							}
							i += 1
						}
					case None =>
				}
			}
			case RemoveLightSourceTask(lightSource, pos) => {
				Noto.info("removing light source " + lightSource)
				localComputors.find(_.canHandleLightSource(lightSource)) match {
					case Some(computor) => computor.removeLightSource(world, lightSource, pos)
					case None => Noto.warn("No computor available to remove light source : " + lightSource)
				}
			}
			case _ =>
				return false
		}
		true
	}

	def addNewLightSource(lightSource: TLightSource) {
		world.aux[GameEventData].onEvent {
			case LightPositionChanged(oldPos, newPos) =>
				messageQueuer(MoveLightSourceTask(lightSource, oldPos, newPos))
				true
			case LightStrengthChanged(oldL, newL) =>
				messageQueuer(UpdateLightStrengthTask(lightSource, oldL, newL))
				true
		}
		localComputors.find(_.canHandleLightSource(lightSource)) match {
			case Some(computor) => computor.addLightSource(world, lightSource)
			case None =>
		}
	}
}


class WorkerActor(world: World, globalComputor: TGlobalLightComputor, localComputors: List[TLocalLightComputor]) extends Actor {
	val subWorker = new LightingWorker(world, globalComputor, localComputors, (m) => self.tell(m, self))

	def name = "Light Component : Worker Actor"

	def receive: Actor.Receive = {
		case "exit" => context.stop(self)
		case other =>
			if (!subWorker.processWork(other)) {
				unhandled(other)
			}
	}


}

object LightingWorker {
	val primaryQ = Bottom
	val gli = 0
}