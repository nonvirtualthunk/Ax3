package arx.bol.game.entities.data

/**
  * TODO: Add javadoc
  */

import java.awt.Shape

import arx.Prelude._
import arx.core.datastructures.FiniteGrid2D
import arx.core.geometry.Rectangle
import arx.core.math.Recti
import arx.core.traits.ArxEnum
import arx.core.traits.ArxEnumObject
import arx.core.traits.TArxEnum
import arx.core.vec.ReadVec2i
import arx.engine.data.TGameEntityAuxData

import scalaxy.loops._

class ItemData extends TGameEntityAuxData {
	var shape : Shape = Rectangle(1,1)

	var bodySlotKind : BodySlotFilter = Hand
}

class WeaponData extends TGameEntityAuxData {
	var baseDamage = 1

	var speedModifier = 1.0f
	var speedBonus = 0.0.seconds
	var damageTypes = List(DamageType.Bludgeoning)
}

class DamageType(nomen : String) extends ArxEnum(nomen)
object DamageType extends ArxEnumObject[DamageType] {
	val Bludgeoning = DamageType("Bludgeoning")
	val Slashing = DamageType("Slashing")
	val Piercing = DamageType("Piercing")
	val Fire = DamageType("Fire")
}

trait BodySlotFilter {
	def matches (bodySlot : BodySlot) : Boolean
}

class BodySlot(val name : String, val kind : BodySlotKind) {

}

class BodySlotKind extends BodySlotFilter {
	override def matches(bodySlot: BodySlot) = bodySlot.kind == this
}

object Hand extends BodySlotKind {

}
object Foot extends BodySlotKind {

}
object Chest extends BodySlotKind {

}



abstract class Shape {
	def bounds : Recti
	def isOccupied(x : Int, y : Int) : Boolean
}

case class Rectangle(w : Int, h : Int) extends Shape {
	val bounds = Recti(0,0,w,h)

	override def isOccupied(x: Int, y: Int) = x >= 0 && y >= 0 && x < w && y < h
}

case class ShapeUnion(shape1 : Shape, pos1 : ReadVec2i, shape2 : Shape, pos2 : ReadVec2i) extends Shape {
	val min = (shape1.bounds.min + pos1).min(shape2.bounds.min + pos2)
	val max = (shape1.bounds.max + pos1).max(shape2.bounds.max + pos2)
	val bounds = Recti(min.x,min.y,max.x - min.x,max.y - min.y)

	override def isOccupied(x: Int, y: Int): Boolean =
		shape1.isOccupied(x - pos1.x, y - pos1.y) || shape2.isOccupied(x - pos2.x, y - pos2.y)
}

//	val occupied = FiniteGrid2D((x,y) =>
//		(shape1.occupied.contains(x,y) && shape1.occupied(x,y)) ||
//		(shape2.occupied.contains(x,y) && shape2.occupied(x,y)),
//		shape1.occupied.width.max(shape2.occupied.width),
//		shape1.occupied.height.max(shape2.occupied.height))
