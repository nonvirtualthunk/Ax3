package arx.eldr.game.logic.physics

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.eldr.game.entity.data.InventoryData
import arx.eldr.game.entity.data.PhysicalData
import arx.engine.entity.GameEntity
import arx.engine.entity.TGameEntity
import arx.engine.world.World
import scalaxy.loops._

class PhysicalEntityGameComponent {

}

object PhysicalEntityGameComponent {
	def entitiesInRegion(world : World, region : VoxelRegion) = {
		var ret : Set[TGameEntity] = Set()
		for (ent <- world.auxDataQuery[PhysicalData]) {
			var effectiveEnt = ent
			while (ent[PhysicalData].heldBy.isDefined) {
				effectiveEnt = ent[PhysicalData].heldBy.get
			}
			if (region.intersects(effectiveEnt[PhysicalData].occupiedRegion)) {
				ret += ent
			}

		}
		ret
	}

	def storeEntityInInventory(entity : TGameEntity, container : TGameEntity) = {
		if (! entity.hasAuxData[PhysicalData]) {
			Noto.warn(s"entity being transferred to inventory is not currently a physical entity : ${entity}")
		}
		entity[PhysicalData].heldBy match {
			case Some(previousContainer) =>
				previousContainer.auxDataOpt[InventoryData] match {
					case Some(inv) =>
						inv.heldEntities -= entity
					case None =>
						Noto.warn(s"entity being held by another entity (${previousContainer}) that does not have inventory data")
				}
			case None =>
		}

		container.auxDataOpt[InventoryData] match {
			case Some(inv) =>
				inv.heldEntities += entity
				entity[PhysicalData].heldBy = Some(container)
				true
			case None =>
				Noto.warn(s"attempting to transfer entity to new container (${container}), but container does not have inventory data")
				false
		}
	}
}
