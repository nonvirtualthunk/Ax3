package arx.bol.control.components

/**
  * TODO: Add javadoc
  */

import arx.bol.game.components.BOL
import arx.bol.game.entities.ItemArchetype
import arx.bol.game.entities.data.ItemData
import arx.bol.game.entities.data.PhysicalData
import arx.bol.game.graphics.data.BagOfLegendGraphicsData
import arx.bol.game.graphics.data.DungeonCrawlGraphicsData
import arx.core.vec.Vec2i
import arx.engine.control.ControlEngine
import arx.engine.control.components.ControlComponent
import arx.engine.control.event.Event.MousePressEvent

class DungeonCrawlControlComponent(engine: ControlEngine) extends ControlComponent(engine) {

	val GD = graphics[DungeonCrawlGraphicsData]
	val entityQuery = world.auxDataQuery[PhysicalData]
	val bol = BOL.bagOfLegend(world)
	val BOLGD = graphics[BagOfLegendGraphicsData]

	controlEvents.onEvent {
		case MousePressEvent(button, pos, mods) =>
			for (clickedPos <- GD.pov.unprojectAtZ(pos, 0.0f, GD.viewport)) {
				bol.leftHand match {
					case Some(heldItem) =>
					case None =>
						entityQuery.results.filter(e => e.archetypeIsA(ItemArchetype)).find(entity => {
							val PD = entity[PhysicalData]

							if (PD.location.x <= clickedPos.x && PD.location.y <= clickedPos.y &&
								PD.location.x + PD.size.x >= clickedPos.x && PD.location.y + PD.size.y >= clickedPos.y) {
								if (entity.archetypeIsA(ItemArchetype)) {
									bol.leftHand = Some(entity)
									BOLGD.leftGrabSlotOffset = entity[ItemData].shape.bounds.dimensions / 2

									world.removeEntity(entity)
									true
								} else {
									false
								}
							} else {
								false
							}
						})
				}
			}
	}
}
