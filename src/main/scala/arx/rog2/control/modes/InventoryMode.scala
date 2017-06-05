package arx.rog2.control.modes

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 6/4/17
  * Time: 6:00 PM
  */

import arx.Prelude._
import arx.core.Moddable
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion

import scalaxy.loops._
import arx.core.vec._
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.widgets.{DimensionExpression, DynamicWidget, PositionExpression, TextDisplayWidget}
import arx.engine.control.event.Event.KeyPressEvent
import arx.engine.entity.TGameEntity
import arx.engine.graphics.data.GraphicsWorld
import arx.graphics.helpers.HSBA
import arx.rog2.game.actions.{EquipItemAction, PlaceItemAction}
import arx.rog2.game.data.entity.BodySlotQualifier.Left
import arx.rog2.game.data.entity.BodySlotType.Hand
import arx.rog2.game.data.entity.{BodySlot, Equippable, Inventory, Physical}
import arx.rog2.graphics.data.{Overlay, OverlayRegion}
import org.lwjgl.glfw.GLFW

class InventoryMode(desktop: Widget, player : TGameEntity) extends ControlMode {
	var invW : Widget = _

	override def initialize(): Unit = {
		invW = new DynamicWidget[List[TGameEntity]](desktop) {
			override def currentContent: List[TGameEntity] = player[Inventory].heldItems
			override def generateWidgets(items: List[TGameEntity]): List[Widget] = {
				items.zipWithIndex.map{ case (item,index) => {
					val itemW = new TextDisplayWidget(this)
					itemW.text = Moddable(() => s"${index+1} : ${item.name}")
					itemW.fontScale = 2.0f
					itemW
				}}
			}
			override def positionWidgets(widgets: List[Widget]): Unit = {
				widgets.foreach(w => w.x = PositionExpression.Constant(10))
				if (widgets.nonEmpty) {
					widgets.head.y = PositionExpression.Constant(10)
				}
				if (widgets.size > 1) {
					widgets.sliding(2).foreach(pair => pair.last.y = PositionExpression.Relative(pair.head,0,Cardinals.Down))
				}
			}
		}
		invW.x = PositionExpression.Centered
		invW.y = PositionExpression.Centered
		invW.width = DimensionExpression.Proportional(0.5f)
		invW.height = DimensionExpression.Proportional(0.75f)
		invW.showing = Moddable(() => this.isTopLevel)
	}


	def indexFromKey(key : Int) = {
		key match {
			case GLFW.GLFW_KEY_0 =>
				Some(9)
			case ni : Int if ni >= GLFW.GLFW_KEY_1 && ni <= GLFW.GLFW_KEY_9 =>
				Some(ni - GLFW.GLFW_KEY_1)
			case ai : Int if ai >= GLFW.GLFW_KEY_A && ai <= GLFW.GLFW_KEY_Z =>
				Some(ai - GLFW.GLFW_KEY_A + 10)
			case _ =>
				None
		}
	}



	onEvent {
		case KeyPressEvent(key,modifiers,repeat) =>
			if (isTopLevel) {
				indexFromKey(key) match {
					case Some(idx) =>
						val items = player[Inventory].heldItems
						if (items.size > idx) {
							val chosen = items(idx)

							var possibilities = Map[String, () => Unit]()
							if (chosen.hasAuxData[Equippable]) {
								possibilities += "[E]quip" -> (() => {
									characterControl.creatureComp.executeAction(EquipItemAction(player, chosen, BodySlot(Hand, Left)))
								})
							}
							if (chosen.hasAuxData[Physical]) {
								possibilities += "[P]lace" -> (() => {
									characterControl.pushMode(new SelectSquareMode(player[Physical].position.plusX(1), (v) => (player[Physical].position - v).abs.allLEQ(1),(v) => {
										characterControl.creatureComp.executeAction(PlaceItemAction(player, chosen, v))
										characterControl.popMode()
									}))
								})
							}
						}
					case None =>
				}
			}
	}

	override def close(): Unit = {
		invW.close()
	}
}


class SelectSquareMode(default : VoxelCoord, criteria : (VoxelCoord) => Boolean, callback : (VoxelCoord) => Unit) extends ControlMode {
	var selected = default

	onEvent {
		case KeyPressEvent(key,modifiers,isRepeat) =>
			if (key == GLFW.GLFW_KEY_ENTER) {
				if (criteria(selected)) {
					callback(selected)
					characterControl.graphics[Overlay].overlaidRegions -= "selection"
					characterControl.popMode()
				}
			} else {
				val vec = keyToVector(key)
				if (vec.lengthSafe > 0.0f) {
					selected = selected + vec
					updateSelectionOverlay()
				}
			}
	}

	def updateSelectionOverlay(): Unit = {
		val isValid = criteria(selected)
		val image = if (isValid) { "rog/ui/overlay/targetPositiveSolid.png" } else { "rog/ui/overlay/targetNegativeSolid.png" }
		characterControl.graphics[Overlay].overlaidRegions += "selection" -> OverlayRegion(VoxelRegion(selected), image, HSBA.White)
	}

	override def initialize(): Unit = {
		updateSelectionOverlay()
	}

	override def close(): Unit = {}
}