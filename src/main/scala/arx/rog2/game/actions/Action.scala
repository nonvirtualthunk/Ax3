package arx.rog2.game.actions

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.control.event.Event.Event
import arx.engine.entity.TGameEntity
import arx.engine.world.World
import arx.rog2.game.data.entity._
import arx.rog2.game.data.world.Terrain
import arx.rog2.game.data.world.Terrain.Modification
import arx.rog2.game.entities.Material
import arx.rog2.game.events._

abstract class Action(val actor: TGameEntity) {
	def isValid(world: World): Boolean

	def timeRequired(world: World): UnitOfTime

	def apply(world: World) : List[Event]
}


case class MoveAction(entity: TGameEntity, from: VoxelCoord, to: VoxelCoord) extends Action(entity) {
	override def isValid(world: World): Boolean = {
		val T = world[Terrain]
		if (entity[Physical].position == from && T.voxel(to).isSentinel) {
			if (from.x == to.x || from.y == to.y) {
				true
			} else {
				// diagonal case, to move diagonal there must be an actual cardinal path between the two points
				T.voxel(VoxelCoord(from.x, to.y, to.z)).isSentinel || T.voxel(VoxelCoord(to.x, from.y, to.z)).isSentinel
			}
		} else {
			false
		}
	}

	override def timeRequired(world: World): UnitOfTime = 1.second

	override def apply(world: World) = {
		val from = entity[Physical].position
		entity[Physical].position = to
		world[Terrain].modifications ::= Modification(to.x,to.y,to.z,Material.Sentinel,Material.Sentinel,world[Terrain].modificationCount)
		world[Terrain].modificationCount += 1

		List(EntityMovedEvent(entity, from, to))
	}
}

case class AttackAction(entity: TGameEntity, target: TGameEntity) extends Action(entity) {
	override def isValid(world: World): Boolean = target[Creature].hp > 0 && world.containsEntity(target)

	override def timeRequired(world: World): UnitOfTime = 1.second

	override def apply(world: World) = {
		val dmg = entity[Creature].damageDealt
		target[Creature].damageTaken += dmg
		if (target[Creature].damageTaken >= target[Creature].maxHP) {
			world.removeEntity(target)
		}
		List(EntityAttackedEvent(entity, target, dmg))
	}
}


case class InteractAction(entity: TGameEntity, target: TGameEntity) extends Action(entity) {
	override def isValid(world: World): Boolean = world.containsEntity(target)

	override def timeRequired(world: World): UnitOfTime = 1.second

	override def apply(world: World) = {
		if (target.hasAuxData[Furniture]) {
			if (target.hasAuxData[Inventory]) {
				target[Inventory].heldItems.headOption match {
					case Some(item) =>
						entity[Inventory].hold(item)
						List(EntityTookItemEvent(entity, item))
					case _ =>
						Nil
				}
			} else {
				Nil
			}
		} else {
			entity[Inventory].hold(target)
			List(EntityTookItemEvent(entity, target))
		}
	}
}

case class PlaceItemAction(actorE: TGameEntity, entity : TGameEntity, location : VoxelCoord) extends Action(actorE) {
	override def isValid(world: World): Boolean = actor[Inventory].heldItems.contains(entity)

	override def timeRequired(world: World): UnitOfTime = 1.second

	override def apply(world: World): List[Event] = {
		actor[Inventory].remove(entity)
		entity[Physical].position = location
		List(EntityPlacedItemEvent(actor, entity, location))
	}
}

case class EquipItemAction(actorE : TGameEntity, entity : TGameEntity, toSlot : BodySlot) extends Action(actorE) {
	override def isValid(world: World): Boolean = actorE.auxDataOpt[Equipper] match {
		case Some(equipper) => equipper.slots.contains(toSlot) && !equipper.equippedEntities.contains(toSlot)
		case None => false
	}

	override def timeRequired(world: World): UnitOfTime = 1.second

	override def apply(world: World): List[Event] = {
		actor[Equipper].equip(entity, toSlot)
		List(EntityEquippedItemEvent(actor, entity, toSlot))
	}
}

case class DoNothingAction(actorE : TGameEntity) extends Action(actorE) {
	override def isValid(world: World): Boolean = true

	override def timeRequired(world: World): UnitOfTime = 1.seconds

	override def apply(world: World): List[Event] = Nil
}