package arx.rog2.game.data.entity

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto

import scalaxy.loops._
import arx.rog2.game.data.world._
import arx.rog2.game.data.entity._
import arx.engine.entity.TGameEntity
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec._
import arx.engine.data.TGameEntityAuxData
import arx.engine.entity.GameEntity

class Inventory extends TGameEntityAuxData {
	var self : TGameEntity = GameEntity.Sentinel
	var heldItems = List[TGameEntity]()

	var maxSpace = 10

	def canHold (entity : TGameEntity) = {
		heldItems.size < maxSpace
	}

	def hold (entity : TGameEntity): Boolean = {
		if (canHold(entity)) {
			if (entity.hasAuxData[Physical]) {
				for (prev <- entity[Physical].heldIn) {
					prev[Inventory].heldItems = prev[Inventory].heldItems.without(entity)
				}
				heldItems ::= entity
				entity[Physical].heldIn = Some(self)
			} else {
				Noto.error(s"Attempting to hold non-physical entity ${entity.name}, id: ${entity.id}")
			}
			true
		} else {
			false
		}
	}

	def remove (entity : TGameEntity): Unit = {
		if (entity.hasAuxData[Physical]) {
			heldItems = heldItems.without(entity)
			entity[Physical].heldIn = None
		} else {
			Noto.error(s"Attempting to hold non-physical entity ${entity.name}, id: ${entity.id}")
		}
	}

	override def onAssignedToEntity(entity: TGameEntity): Unit = {
		self = entity
	}
}
