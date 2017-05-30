package arx.rog2.game.engine

/**
  * TODO: Add javadoc
  */

import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.data.TimeData
import arx.engine.entity.TGameEntity
import arx.engine.game.GameEngine
import arx.engine.game.components.GameComponent
import arx.engine.game.events.EntityAddedEvent
import arx.rog2.game.data.entity.Physical
import arx.rog2.game.data.world.EntityOcclusionData
import arx.Prelude._
import arx.engine.game.events.EntityRemovedEvent
import arx.rog2.game.events.{EntityMovedEvent, EntityPlacedItemEvent}

import scalaxy.loops.rangeExtensions

class RogMainGameComponent(engine: GameEngine) extends GameComponent(engine) {
	gameEvents.onEvent {
		case EntityAddedEvent(ent) if ent.hasAuxData[Physical] =>
			updateEntityLocation(ent, None, stillPresent = true)
		case EntityMovedEvent(ent, from, to) if ent.hasAuxData[Physical] =>
			updateEntityLocation(ent, Some(from), stillPresent = true)
		case EntityRemovedEvent(ent) if ent.hasAuxData[Physical] =>
			updateEntityLocation(ent, Some(ent[Physical].position), stillPresent = false)
		case EntityPlacedItemEvent(_, ent, to) if ent.hasAuxData[Physical] =>
			updateEntityLocation(ent, None, stillPresent = true)
	}

	override protected def initialize(): Unit = {
		world[TimeData].pause()
	}

	override protected def update(dt: UnitOfTime): Unit = {

	}

	def updateEntityLocation(ent: TGameEntity, movedFrom: Option[VoxelCoord], stillPresent: Boolean): Unit = {
		val svo = world[EntityOcclusionData].entities
		val PD = ent[Physical]
		if (PD.dimensions.x >= 1.voxel && PD.dimensions.y >= 1.voxel && PD.dimensions.z >= 1.voxel) {
			for (from <- movedFrom) {
				for (x <- from.x until from.x + PD.dimensions.x.inVoxels.toInt optimized;
					  y <- from.y until from.y + PD.dimensions.y.inVoxels.toInt optimized;
					  z <- from.z until from.z + PD.dimensions.z.inVoxels.toInt optimized) {
					svo.remove(x, y, z, ent)
				}
			}
			if (stillPresent) {
				PD.forEveryVoxelInShape((x, y, z) => svo.set(x, y, z, ent))
			}
		}
	}
}
