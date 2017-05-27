package arx.bol.control.components

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.bol.game.components.BOL
import arx.bol.game.entities.data.InventorySlot
import arx.bol.game.entities.data.ItemData
import arx.bol.game.entities.data.NoInventorySlot
import arx.bol.game.graphics.data.BagOfLegendGraphicsData
import arx.bol.game.graphics.data.SlotHighlight
import arx.core.geometry.Plane
import arx.core.math.Intersection
import arx.core.math.Recti
import arx.core.units.UnitOfTime
import arx.core.vec.ReadVec2f
import arx.core.vec.Vec3f
import arx.core.vec.Vec3f
import arx.core.vec.Vec4f
import arx.engine.EngineCore
import arx.engine.control.ControlEngine
import arx.engine.control.components.ControlComponent
import arx.engine.control.event.Event.Mouse
import arx.engine.control.event.Event.MouseMoveEvent
import arx.engine.control.event.Event.MousePressEvent

import scalaxy.loops._

class InventoryControlComponent(engine : ControlEngine) extends ControlComponent(engine) {

	def effectivePos (pos : ReadVec2f) = {
		val IGD = graphics[BagOfLegendGraphicsData]
		val rect = Recti(0,0,EngineCore.pixelWidth, EngineCore.pixelHeight - 800)
		IGD.pov.unprojectAtZ(pos, 0.0f, rect)
	}
	def mouseToSlot(pos : ReadVec2f) = effectivePos(pos).map(v => BOL.bagOfLegend(world).slot(v.x.round, v.y.round))



	controlEvents.onEvent {
		case MousePressEvent(button,pos,modifiers) =>
			val bol = BOL.bagOfLegend(world)

			mouseToSlot(pos) match {
				case Some(clickedSlot) =>
					val IGD = graphics[BagOfLegendGraphicsData]
					bol.leftHand match {
						case Some(item) =>
							val off = IGD.leftGrabSlotOffset
							if (bol.canHoldItem(item, clickedSlot.position.x - off.x, clickedSlot.position.y - off.y)) {
								bol.holdItem(item, clickedSlot.position.x - off.x, clickedSlot.position.y - off.y)
								bol.leftHand = None
							}
						case None =>
							for (itemInSlot <- clickedSlot.occupiedBy) {
								val effPos = effectivePos(pos).getOrElse(Vec3f.Zero)

								IGD.leftGrabSlotOffset = clickedSlot.position - bol.relativePositionOf(itemInSlot)
								bol.leftHand = Some(itemInSlot)
								bol.removeItem(itemInSlot)
							}
					}
				case None => Noto.error("wat")
			}
		case MouseMoveEvent(button, pos, modifiers) =>
			val bol = BOL.bagOfLegend(world)

			mouseToSlot(pos) match {
				case Some(clickedSlot) =>
				case None => Noto.error("wat?")
			}
	}

	override protected def update(dt: UnitOfTime): Unit = {
		val bol = BOL.bagOfLegend(world)

		Mouse.setVisible(bol.leftHand.isEmpty)

		mouseToSlot(Mouse.currentPosition) match {
			case Some(clickedSlot) =>
				val IGD = graphics[BagOfLegendGraphicsData]
				bol.leftHand match {
					case Some(heldItem) =>

						var slots = Set[InventorySlot]()
						val shape = heldItem[ItemData].shape
						val off = IGD.leftGrabSlotOffset
						for (x <- shape.bounds.minX until shape.bounds.maxX ; y <- shape.bounds.minY until shape.bounds.maxY) {
							if (shape.isOccupied(x,y)) {
								slots += bol.slot(clickedSlot.position.x - off.x + x, clickedSlot.position.y - off.y + y)
							}
						}
						val color = if (bol.canHoldItem(heldItem, clickedSlot.position.x - off.x, clickedSlot.position.y - off.y)) {
							Vec4f(0.2f,0.8f,0.2f,1.0f)
						} else {
							Vec4f(0.8f,0.2f,0.2f,1.0f)
						}
						IGD.slotHighlights += "might-place" -> SlotHighlight(slots,color)
					case _ =>
						IGD.slotHighlights -= "might-place"
				}
			case _ =>
		}
	}
}
