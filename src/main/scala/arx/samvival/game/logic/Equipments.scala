package arx.samvival.game.logic

import arx.engine.lworld.{LEntity, LWorld}
import arx.samvival.game.entities.Equipment
import arx.samvival.game.entities.Fields.Equipment
import arx.core.introspection.FieldOperations._
import arx.samvival.game.events.GameEvents.EquipItem

object Equipments {


	def equip (entity : LEntity, item : LEntity)(implicit world : LWorld) : Boolean = {
		world.startEvent(EquipItem(entity, item))
		world.modify(entity, Equipment.equipped + item, "equipped")
		world.endEvent(EquipItem(entity, item))
		true
	}



}
