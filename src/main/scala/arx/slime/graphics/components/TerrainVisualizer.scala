package arx.slime.graphics.components

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.units.UnitOfTime
import arx.core.vec.Vec2f
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.GraphicsComponent
import arx.graphics.AVBO
import arx.graphics.TextureBlock
import arx.graphics.VBO
import arx.resource.ResourceManager
import arx.slime.game.archetypes.NoTerrain
import arx.slime.game.core.HexCoord
import arx.slime.game.data.Terrain
import arx.slime.graphics.core.HexAttributeProfile
import org.lwjgl.opengl.GL11

import scalaxy.loops._

class TerrainVisualizer(ge: GraphicsEngine) extends GraphicsComponent(ge) {
	val tb = new TextureBlock(1024, 1024)
	tb.minFilter = GL11.GL_NEAREST
	tb.magFilter = GL11.GL_NEAREST
	val vbo = new AVBO(HexAttributeProfile)
	lazy val shader = ResourceManager.shader("slime/shaders/HexMap")

	drawPriority = 10

	override def draw(): Unit = {
		tb.bind()
		shader.bind()

		pov.look()

		vbo.solidifyIfNecessary()
		vbo.drawElements()
	}


	override protected def update(dt: UnitOfTime): Unit = {
		if (vbo.changeState(VBO.Dirty, VBO.Updating)) {
			val width = 1.0f
			val height = sqrtf(3.0f) / 2.0f * width

			val offsets = Vector(
				Vec2f(-0.5f * width, 0.0f), Vec2f(-0.25f * width, 0.5f * height), Vec2f(0.25f * width, 0.5f * height),
				Vec2f(0.5f * width, 0.0f), Vec2f(0.25f * width, -0.5f * height), Vec2f(-0.25f * width, -0.5f * height))

			val texCoordOffsets = Vector(
				Vec2f(0.0f,0.5f), Vec2f(0.25f,1.0f), Vec2f(0.75f,1.0f),
				Vec2f(1.0f,0.5f), Vec2f(0.75f,0.0f), Vec2f(0.25f,0.0f)
			)

			val pb = HexAttributeProfile.createPointBuilder()

			val TD = world[Terrain]
			for (x <- -TD.radius to TD.radius optimized;
				  y <- -TD.radius to TD.radius optimized) {
				val terrain = TD(HexCoord(x, y))
				terrain match {
					case NoTerrain => // do nothing
					case tt =>
						val vi = vbo.incrementVertexOffset(7)
						val ii = vbo.incrementIndexOffset(18)

						val tc = tb(ResourceManager.image("slime/terrain/" + terrain.name + ".png"))
						val tcW = tc(2).x - tc(0).x
						val tcH = tc(2).y - tc(0).y

						val center = hexToCoord(x,y)
						for (q <- 0 until 6 optimized) {
							pb.setV(center.x + offsets(q).x, center.y + offsets(q).y,0.0f)
//							pb.setC(tt.color)
							pb.setC(1.0f,1.0f,1.0f,1.0f);
							pb.setTC(tc(0).x + texCoordOffsets(q).x * tcW, tc(0).y + texCoordOffsets(q).y * tcH)
							pb.setE(1.0f)
							vbo.setPoint(vi+q,pb)
						}
						pb.setV(center.x,center.y,0.0f)
//						pb.setC(tt.color)
						pb.setC(1.0f,1.0f,1.0f,1.0f);
						pb.setTC(tc(0).x + tcW * 0.5f, tc(0).y + tcH * 0.5f)
						pb.setE(0.0f)
						vbo.setPoint(vi+6,pb)

						setIHex(vbo, vi, ii)
				}
			}

			vbo.changeState(VBO.Updating, VBO.Updated)
		}
	}

	def hexToCoord(x: Int, y: Int) = {
		Vec2f(0.5f * 1.5f * x, 0.5f * sqrtf(3.0f) * (y + x * 0.5f))
	}

	def setIHex(vbo : AVBO, vi : Int, ii : Int) = {
//		vbo.setI(ii+0,vi+0)
//		vbo.setI(ii+1,vi+1)
//		vbo.setI(ii+2,vi+5)
//
//		vbo.setI(ii+3,vi+1)
//		vbo.setI(ii+4,vi+2)
//		vbo.setI(ii+5,vi+5)
//
//		vbo.setI(ii+6,vi+2)
//		vbo.setI(ii+7,vi+4)
//		vbo.setI(ii+8,vi+5)
//
//		vbo.setI(ii+9,vi+2)
//		vbo.setI(ii+10,vi+3)
//		vbo.setI(ii+11,vi+4)
		for (q <- 0 until 6 optimized) {
			vbo.setI(ii+q*3+0,vi+q+0)
			vbo.setI(ii+q*3+1,vi+(q+1)%6)
			vbo.setI(ii+q*3+2,vi+6)
		}
	}
}
