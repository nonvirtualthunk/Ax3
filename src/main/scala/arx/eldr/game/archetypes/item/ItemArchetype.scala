package arx.eldr.game.archetypes.item

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.traits.TSentinel
import arx.core.units.Dimensions
import arx.core.vec.Vec3f
import arx.eldr.game.archetypes.InputKind
import arx.eldr.game.archetypes.Reaction.InputMap
import arx.eldr.game.entity.data.ItemData
import arx.eldr.game.entity.data.ItemFlag
import arx.eldr.game.entity.data.PhysicalData
import arx.eldr.game.entity.data.SubVoxelShape
import arx.engine.entity.GameArchetype
import arx.engine.entity.GameEntity
import arx.engine.entity.TArchetypeKind
import arx.engine.entity.TGameEntity

import scalaxy.loops._

abstract class ItemArchetype(nomen : String) extends GameArchetype(nomen, ItemArchetype){
	def createItem(constructedFrom : List[TGameEntity]) : TGameEntity = {
		val ent = new GameEntity(nomen)
		ent.archetype = this
		ent.copyDataFrom[ItemData](this)
		ent.copyDataFrom[PhysicalData](this)

		ent
	}
}

object ItemArchetype extends TArchetypeKind {
	val Sentinel : ItemArchetype = new ItemArchetype("Sentinel") with TSentinel
}

object StoneHatchet extends ItemArchetype("stone hatchet") {
	val ID = aux[ItemData]
	ID.flags += ItemFlag.DurableEdge
	ID.durability = MM(100.0f)

	val PD = aux[PhysicalData]
	PD.occupiesSpace = false
	PD.shape = SubVoxelShape(new Dimensions(1.meter,0.2.meter,0.05.meter))

}
