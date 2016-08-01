package arx.slime.graphics.components

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.core.units.UnitOfTime
import arx.core.vec.Vec2f
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.CanvasGraphicsComponent
import arx.engine.simple.Canvas
import arx.resource.ResourceManager
import arx.slime.game.data.PhysicalData
import org.lwjgl.opengl.GL11

class EntityVisualizer(ge : GraphicsEngine) extends CanvasGraphicsComponent(ge) {
	val query = world.auxDataQuery[PhysicalData]
	canvas.useTexFilters(GL11.GL_NEAREST,GL11.GL_NEAREST)

	override def draw(canvas: Canvas): Unit = {
		for (ent <- query) {
			ent.archetype match {
				case Some(arch) =>
					val base = arch.name.stripWhitespace.toCamelCase
					val eff = base.substring(0,1).toLowerCase() + base.substring(1)

					val img = ResourceManager.image("slime/entities/" + eff + "/image.png")

					val pos = ent[PhysicalData].position.toCartesian
					canvas.drawQuad(pos,Vec2f(0.7f),img)
				case None => Noto.warn("Physical entity with no archetype")
			}
		}
	}

	override protected def update(dt: UnitOfTime): Unit = {

	}
}
