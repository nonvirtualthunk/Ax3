package arx.rog2.control.modes

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 6/4/17
  * Time: 6:10 PM
  */

import arx.Prelude._

import scalaxy.loops._
import arx.core.vec._
import arx.engine.control.data.WindowingData
import arx.engine.control.event.Event.KeyPressEvent
import arx.engine.entity.TGameEntity
import arx.engine.world.World
import arx.rog2.game.actions.{AttackAction, InteractAction, MoveAction, PlaceItemAction}
import arx.rog2.game.data.entity.{Creature, Inventory, Physical}
import arx.rog2.game.data.world.{RogData, Terrain}
import org.lwjgl.glfw.GLFW
import org.lwjgl.glfw.GLFW._

class MainMode extends ControlMode {
	var lastMove = 0.seconds
	val gapBetweenRepeat = 0.1.seconds



	onEvent {
		case KeyPressEvent(key, modifiers, repeat) => {
			val cc = characterControl
			import cc._

			val desktop = control[WindowingData].desktop
			if (key == GLFW_KEY_I && isTopLevel) {
				cc.pushMode(new InventoryMode(desktop, player))
			}

			if (!repeat || (curTime() - lastMove) > gapBetweenRepeat) {
				lastMove = curTime()
				val movement = keyToVector(key)

				if (movement != Vec3i.Zero) {
					val PD = world[RogData].player[Physical]
					val targetPos = PD.position + movement

					physics.entitiesAtLocation(targetPos).filter(e => e[Physical].solid) match {
						case Nil =>
							val T = world[Terrain]
							if (T.voxel(targetPos).isSentinel) {
								creatureComp.executeAction(MoveAction(player, PD.position, targetPos))
							} else if (T.voxel(targetPos.plusZ(1)).isSentinel) {
								creatureComp.executeAction(MoveAction(player, PD.position, targetPos.plusZ(1)))
							}
						case entities =>
							val intersectedEntity = entities.head
							if (intersectedEntity.hasAuxData[Creature]) {
								creatureComp.executeAction(AttackAction(player, intersectedEntity))
							} else {
								creatureComp.executeAction(InteractAction(player, intersectedEntity))
							}
					}
				} else {
					key match {
						case GLFW_KEY_P =>
							for (heldItem <- player[Inventory].heldItems.headOption) {
								val placePos = player[Physical].position + Cardinals.dirvec(player[Physical].facing)
								creatureComp.executeAction(PlaceItemAction(player, heldItem, placePos))
							}
						case _ =>
					}
				}
			}
		}
	}


	override def initialize(): Unit = {

	}

	override def close(): Unit = {}
}
