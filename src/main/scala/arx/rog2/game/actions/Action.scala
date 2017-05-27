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
import arx.rog2.game.EntityAttackedEvent
import arx.rog2.game.EntityMovedEvent
import arx.rog2.game.data.entity.Creature
import arx.rog2.game.data.entity.Furniture
import arx.rog2.game.data.entity.Inventory
import arx.rog2.game.data.entity.Physical
import arx.rog2.game.data.world.Terrain
import arx.rog2.game.data.world.Terrain.Modification
import arx.rog2.game.entities.Material

abstract class Action(entity: TGameEntity) {
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
				for (item <- target[Inventory].heldItems.headOption) {
					entity[Inventory].hold(item)
				}
			}
		} else {
			entity[Inventory].hold(target)
		}
		List()
	}
}

case class PlaceItemAction(actor: TGameEntity, entity : TGameEntity, location : VoxelCoord) extends Action(entity) {
	override def isValid(world: World): Boolean = actor[Inventory].heldItems.contains(entity)

	override def timeRequired(world: World): UnitOfTime = 1.second

	override def apply(world: World): List[Event] = {
		actor[Inventory].remove(entity)
		entity[Physical].position = location
		List(EntityMovedEvent(entity, VoxelCoord.Sentinel, location))
	}
}