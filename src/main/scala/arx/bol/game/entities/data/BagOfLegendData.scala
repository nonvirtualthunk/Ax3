package arx.bol.game.entities.data

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.core.datastructures.FiniteGrid2D
import arx.core.math.Recti
import arx.core.traits.TSentinel
import arx.core.traits.TSentinelable
import arx.core.vec.ReadVec2f
import arx.core.vec.ReadVec2i
import arx.core.vec.Vec2i
import arx.engine.data.TGameEntityAuxData
import arx.engine.entity.TGameEntity

import scala.language.postfixOps
import scalaxy.loops._

class BagOfLegendData extends TGameEntityAuxData {
	protected val slots = FiniteGrid2D[InventorySlot](Vec2i(15,20), NoInventorySlot(0,0))
	for (x <- 0 until slots.width; y <- 0 until slots.height) {
		slots(x,y) = new InventorySlot(None,Vec2i(x,y))
	}

	var leftHand = none[TGameEntity]
	var rightHand = none[TGameEntity]

	def slot(x : Int, y : Int) = if (x >= 0 && y >= 0 && x < slots.width && y < slots.height) {
		slots(x,y)
	} else {
		NoInventorySlot(x,y)
	}

	def allSlots = slots

	def slotBounds = Recti(0,0,slots.width,slots.height)

	private var allHeldItems = Set[TGameEntity]()

	def heldItems = allHeldItems

	def canHoldItem(item : TGameEntity, slot : InventorySlot) : Boolean = {
		canHoldItem(item,slot.position.x,slot.position.y)
	}
	def canHoldItem(item : TGameEntity, xOff : Int, yOff : Int) : Boolean = {
		if (!item.hasAuxData[ItemData]) {
			Noto.error("Cannot hold something that is not an item: " + item)
			false
		} else {
			val shape = item[ItemData].shape
			for (x <- shape.bounds.minX until shape.bounds.maxX optimized;
				  y <- shape.bounds.minY until shape.bounds.maxY optimized) {
				if (!slot(x + xOff,y + yOff).canHold(item) && shape.isOccupied(x,y)) {
					return false
				}
			}
			true
		}
	}
	def holdItem(item : TGameEntity, xOff : Int, yOff : Int): Boolean = {
		if (canHoldItem(item,xOff,yOff)) {
			val shape = item[ItemData].shape
			for (x <- shape.bounds.minX until shape.bounds.maxX optimized;
				  y <- shape.bounds.minY until shape.bounds.maxY optimized) {
				if (shape.isOccupied(x,y)) {
					slot(x + xOff,y + yOff).occupiedBy = Some(item)
				}
			}
			allHeldItems += item
			true
		} else {
			false
		}
	}
	def removeItem(item : TGameEntity) = {
		allSlots.foreach(s => s.occupiedBy = s.occupiedBy match {
			case Some(e) if e == item => None
			case o => o
		})
		allHeldItems -= item
	}
	def slotsHolding(item : TGameEntity) = {
		allSlots.filter(s => s.occupiedBy.contains(item))
	}
	def relativePositionOf(item : TGameEntity) : ReadVec2i = {
		slotsHolding(item).map(s => s.position).reduceLeft(_.min(_))
	}
}

class InventorySlot(var occupiedBy : Option[TGameEntity], val position : ReadVec2i) extends TSentinelable {
	def canHold(entity : TGameEntity) = occupiedBy.isEmpty
	def isOccupied = occupiedBy.isDefined
	def isEmpty = ! isOccupied
}

case class NoInventorySlot(x:Int,y:Int) extends InventorySlot(None, Vec2i(x,y)) with TSentinel {
	override def canHold(entity: TGameEntity): Boolean = false
	override def isOccupied: Boolean = false
}