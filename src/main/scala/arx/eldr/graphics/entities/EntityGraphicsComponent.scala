package arx.eldr.graphics.entities

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.{ForwardingModdable, Moddable}
import arx.core.units.UnitOfTime
import arx.core.vec.{Cardinals, Vec2f}
import arx.eldr.game.entity.data.PhysicalData
import arx.eldr.game.world.data.Light
import arx.eldr.graphics.data.EntityGraphicsData
import arx.eldr.graphics.environment.{BillboardAttributeProfile, BillboardShader, WorldAttributeProfile}
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.GraphicsComponent
import arx.graphics.{AVBO, GL, TextureBlock}
import arx.graphics.pov.TCamera
import arx.resource.ResourceManager
import org.lwjgl.opengl.GL11

import scala.language.postfixOps
import scalaxy.loops._

class EntityGraphicsComponent(eng:GraphicsEngine) extends GraphicsComponent(eng) {
	val physicalEntities = world.auxDataQuery[PhysicalData]
	val shader = new BillboardShader(world, new ForwardingModdable(pov _))

	val vbo = new AVBO(BillboardAttributeProfile)
	val textures = new TextureBlock(1024,1024)
	textures.magFilter = GL11.GL_NEAREST
	textures.minFilter = GL11.GL_NEAREST

	override def draw(): Unit = {
		GL.glSetState(GL11.GL_CULL_FACE,enable = false)

		shader.bind()
		textures.bind()
		pov.look()

		vbo.bind()
		vbo.solidifyIfNecessary()
		vbo.drawElements()
	}

	override protected def update(dt: UnitOfTime): Unit = {
		vbo.changeState(AVBO.Clean, AVBO.Dirty)

		val billboardOffsets = Array(Vec2f(-0.5f,0.0f),Vec2f(0.5f,0.0f),Vec2f(0.5f,1.0f),Vec2f(-0.5f,1.0f))
		if (vbo.changeState(AVBO.Dirty, AVBO.Updating)) {
			vbo.clear()

			val light = world[Light].global(0)
			val fullLight = world[Light].globalFullLight(0)

			val pb = BillboardAttributeProfile.createPointBuilder()
			for (ent <- physicalEntities) {
				val PD = ent[PhysicalData]
				var pos = PD.position.toObjectCoordFoot

				ent.auxDataOpt[EntityGraphicsData] match {
					case Some(egd) if egd.graphicalPosition.notSentinel => pos = egd.graphicalPosition
					case _ => // do nothing
				}

				val lightValue = light(PD.position) / fullLight
				val tcs = textures(ResourceManager.image("eldr/entities/animals/human/human.png"))

				val vi = vbo.incrementVertexOffset(4)
				val ii = vbo.incrementIndexOffset(6)

				for (q <- 0 until 4 optimized) {
					pb.setV(pos.x,pos.y,pos.z)
					pb.setGL(1.0f,1.0f,1.0f,lightValue)
					pb.setLL(0.0f,0.0f,0.0f,0.0f)
					pb.setTC(tcs(q))
					pb.setB(billboardOffsets(q).x,billboardOffsets(q).y)
					vbo.setPoint(vi+q,pb)
				}
				vbo.setIQuad(ii,vi)
			}

			if (!vbo.changeState(AVBO.Updating, AVBO.Updated)) {
				Noto.severeError("Entity graphics component could not switch to correct updated state")
			}
		}
	}
}
