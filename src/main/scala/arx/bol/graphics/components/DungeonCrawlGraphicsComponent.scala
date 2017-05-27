package arx.bol.graphics.components

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.bol.game.components.BOL
import arx.bol.game.components.EntityTypes.Creature
import arx.bol.game.entities.PlayerCharacter
import arx.bol.game.entities.data.ActionType
import arx.bol.game.entities.data.Behavior
import arx.bol.game.entities.data.CreatureData
import arx.bol.game.entities.data.PhysicalData
import arx.bol.game.graphics.data.DungeonCrawlGraphicsData
import arx.bol.game.world.data.DungeonData
import arx.core.Moddable
import arx.core.math.Recti
import arx.core.vec.ReadVec2f
import arx.core.vec.Vec2f
import arx.core.vec.Vec3f
import arx.engine.control.event.Event.Mouse
import arx.engine.data.TimeData
import arx.engine.entity.GameArchetype
import arx.engine.entity.GameEntity
import arx.engine.entity.TGameEntity
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.CanvasGraphicsComponent
import arx.engine.graphics.components.GraphicsComponent
import arx.engine.graphics.data.PovData
import arx.engine.simple.Canvas
import arx.graphics.AVBO
import arx.graphics.GL
import arx.graphics.attributeprofiles.SimpleAttributeProfile
import arx.graphics.pov.TopDownCamera
import org.lwjgl.opengl.GL11

import scalaxy.loops._

//class DungeonCrawlComponent(engine : GraphicsEngine) extends GraphicsComponent(engine) {
//	val vbo = new AVBO(SimpleAttributeProfile)
//
//	override def draw(): Unit = {
//		vbo.solidifyIfNecessary()
//		vbo.draw(GL11.GL_TRIANGLES)
//	}
//
//
//}

class DungeonCrawlGraphicsComponent(engine : GraphicsEngine) extends CanvasGraphicsComponent(engine) {
	val entityQuery = world.auxDataQuery[PhysicalData]

	canvas.useTexFilters(GL11.GL_NEAREST,GL11.GL_NEAREST)

	pov = Moddable(graphics[DungeonCrawlGraphicsData].pov)

	val componentHeight = 800
	canvas.viewportOverride = Some((curV: Recti) => Recti(0,curV.height - componentHeight,curV.width,componentHeight))

	override def draw(canvas: Canvas): Unit = {
		val dungeon = world[DungeonData].activeDungeon

		for (entity <- entityQuery) {
			drawEntity(entity, None)
		}

		for (pc <- entityQuery.find(ge => ge.hasAuxData[CreatureData] && ge[CreatureData].behavior == Behavior.PlayerCharacter)) {
			val PD = pc[PhysicalData]
			val td = graphics[DungeonCrawlGraphicsData].pov
			td.eye = Vec3f(PD.location,td.eye.z)
		}

		for (i <- 0 until 100) {
			for (j <- 0 until dungeon.heightmap(i)) {
				canvas.quad(Vec2f(i,j) + 0.5f)
				   .withColor(1.0f,1.0f,1.0f,1.0f)
				   .withDimensions(Vec2f(1.0f,1.0f))
				   .withTexture("bol/world/stone.png")
				   .draw()
			}
		}

		val bol = BOL.bagOfLegend(world)
		for (held <- bol.leftHand) {
			for (pos <- pov.unprojectAtZ(Mouse.currentPosition, 0.0f, viewport)) {
				drawEntity(held, Some(pos.xy))
			}
		}

	}

	def drawEntity(entity : TGameEntity, atPos : Option[ReadVec2f]): Unit = {
		val PD = entity[PhysicalData]

		val base = {entity.archetype.map(a => a.name).getOrElse("")}
		val img = if (entity.hasAuxData[CreatureData]) {
			val creature : Creature = entity

			val index = creature.currentAction match {
				case ActionType.Wait => 0
				case _ => (creature.actionProgress * 4).toInt.min(3)
			}
			s"bol/entities/creatures/$base/${creature.currentAction.name.toLowerCase}/$index.png"
		} else {
			s"bol/entities/items/${base.toSnakeCase}.png"
		}

		val pos = atPos.getOrElse(PD.location + PD.size * 0.5f)

		canvas.quad(pos)
			.withDimensions(PD.size)
			.withColor(1,1,1,1)
			.withTexture(img)
			.draw()
	}
}