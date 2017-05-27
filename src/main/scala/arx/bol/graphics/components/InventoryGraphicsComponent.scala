package arx.bol.graphics.components

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.bol.game.components.BOL
import arx.bol.game.components.EntityTypes.Creature
import arx.bol.game.entities.ItemArchetype
import arx.bol.game.entities.data.BagOfLegendData
import arx.bol.game.entities.data.Behavior
import arx.bol.game.entities.data.CreatureData
import arx.bol.game.entities.data.ItemData
import arx.bol.game.entities.data.PhysicalData
import arx.bol.game.graphics.data.BagOfLegendGraphicsData
import arx.bol.game.world.data.DungeonData
import arx.core.Moddable
import arx.core.math.Recti
import arx.core.vec.ReadVec2f
import arx.core.vec.Vec2f
import arx.core.vec.Vec2i
import arx.core.vec.Vec3f
import arx.core.vec.Vec4f
import arx.engine.control.event.Event.Mouse
import arx.engine.entity.TGameEntity
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.CanvasGraphicsComponent
import arx.engine.simple.Canvas
import arx.graphics.GL
import org.lwjgl.opengl.GL11

import scalaxy.loops._

class InventoryGraphicsComponent(engine : GraphicsEngine) extends CanvasGraphicsComponent(engine) {
	canvas.useTexFilters(GL11.GL_NEAREST,GL11.GL_NEAREST)

	val invQuery = world.auxDataQuery[BagOfLegendData]
	val creatureQuery = world.auxDataQuery[CreatureData]
	def pcQuery = creatureQuery.filter(c => c[CreatureData].behavior == Behavior.PlayerCharacter)

	val itemQuery = world.createEntityPredicateQuery(e => e.archetypeIsA(ItemArchetype))

	val componentHeight = 800
	val viewportOverride = (curV: Recti) => Recti(0,0,curV.width,curV.height - componentHeight)
	canvas.viewportOverride = Some(viewportOverride)

	pov = Moddable(() => graphics[BagOfLegendGraphicsData].pov)

	override def draw(canvas: Canvas): Unit = {
		val bol = BOL.bagOfLegend(world)
		val ID = bol.aux[BagOfLegendData]
		val IGD = graphics[BagOfLegendGraphicsData]
		IGD.viewport = Moddable(viewportOverride(GL.viewport))

		val offset = ID.slotBounds.min + (ID.slotBounds.max - ID.slotBounds.min) * 0.5f - 0.5f
		graphics[BagOfLegendGraphicsData].pov.eye = Vec3f(offset.x,offset.y,pov.eye.z)

		val highlightBySlot = IGD.slotHighlights.values
			.flatMap(h => h.slots.map(s => s -> h.color))
			.toMap

		for (slot <- ID.allSlots) {
			val color = highlightBySlot.getOrElse(slot, Vec4f.One)

			canvas.quad(slot.position)
				.withTexture("bol/entities/inventory/inventory_space.png")
				.withColor(color)
				.withDimensions(1,1)
				.draw()
		}

		val slotsByItem = ID.allSlots.filter(s => s.occupiedBy.isDefined).groupBy(s => s.occupiedBy.get)
		for ((item, slots) <- slotsByItem) {
			val positions = slots.map(s => s.position)
			val minX = positions.map(s => s.x).min
			val minY = positions.map(s => s.y).min
			val maxX = positions.map(s => s.x).max
			val maxY = positions.map(s => s.y).max

			val center = Vec2f((minX + maxX) * 0.5f, (minY + maxY) * 0.5f)

			drawItem(item, center)
		}

		for (held <- bol.leftHand) {
			for (pos <- pov.unprojectAtZ(Mouse.currentPosition, 0.0f, viewport)) {
				drawItem(held, pos.xy - IGD.leftGrabSlotOffset)
			}
		}
	}

	def drawItem(item : TGameEntity, center : ReadVec2f): Unit = {
		val arch = item.archetype.getOrElse(ItemArchetype.Sentinel)

		val dim = item[ItemData].shape.bounds.dimensions

		canvas.quad(center)
			.withDimensions(dim.x, dim.y)
			.withColor(1,1,1,1)
			.withTexture(s"bol/entities/items/${arch.name.toSnakeCase}.png")
			.draw()
	}
}
