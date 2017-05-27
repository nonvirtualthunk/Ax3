package arx.bol.game.entities

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.bol.game.entities.data.ItemData
import arx.bol.game.entities.data.PhysicalData
import arx.bol.game.entities.data.WeaponData
import arx.core.traits.TSentinel
import arx.core.vec.Vec2f
import arx.engine.entity.GameArchetype
import arx.engine.entity.GameEntity
import arx.engine.entity.TArchetypeKind

import scalaxy.loops._

class ItemArchetype(nomen : String) extends GameArchetype(nomen, ItemArchetype) {

	override def create() = {
		val entity = new GameEntity(nomen)
		entity.copyDataFrom[ItemData](this)
		entity.copyDataFrom[WeaponData](this)
		val PD = entity[PhysicalData]
		PD.size = Vec2f(this.aux[ItemData].shape.bounds.dimensions) * 0.5f
		entity.archetype = this

		entity
	}
}

class WeaponArchetype(nomen : String) extends ItemArchetype(nomen) {

}

object ItemArchetype extends TArchetypeKind {
	val Sentinel = new ItemArchetype("Sentinel") with TSentinel

	implicit def toItemData (arch : ItemArchetype) : ItemData = arch[ItemData]
}
object WeaponArchetype extends TArchetypeKind {
	val Sentinel = new WeaponArchetype("Sentinel") with TSentinel

	implicit def toWeaponData (arch : WeaponArchetype) : WeaponData = arch[WeaponData]
	implicit def toPhysicalData (arch : WeaponArchetype) : PhysicalData = arch[PhysicalData]

	override def parentKind: Option[TArchetypeKind] = Some(ItemArchetype)
}