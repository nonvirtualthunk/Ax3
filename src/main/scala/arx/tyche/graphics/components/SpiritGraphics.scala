
package arx.tyche.graphics.components

import arx.Prelude
import arx.Prelude._
import arx.core.math.Interpolation
import arx.core.vec.{ReadVec4f, Vec2f, Vec3f, Vec4f}
import arx.engine.entity.TGameEntity
import arx.engine.graphics.GraphicsEngine
import arx.tyche.core.GlobeCoord
import arx.tyche.game.data.Spirit
import arx.tyche.graphics.components.TyOrientation.{Billboard, Standing}
import arx.tyche.graphics.data.{Particle, Particles}
import org.lwjgl.glfw.GLFW
import org.lwjgl.opengl.GL11

import scala.util.Random

class SpiritGraphics(ge: GraphicsEngine) extends TyGraphicsComponent(ge) {
	lazy val spirits = world.createEntityQuery {
		case e: TGameEntity if e.hasAuxData[Spirit] => e
	}

	var bobbleTime = GLFW.glfwGetTime()

	depthFunc = GL11.GL_LEQUAL
	canvas.textureBlock.magFilter = GL11.GL_NEAREST

	override def draw(canvas: TyCanvas): Unit = {

		var spiritPositions = spirits.map(s => s[Spirit].position.plusZ(0.15f).asCartesian).toList
		while (spiritPositions.size < 4) {
			spiritPositions ::= spiritPositions.head
		}

		shader.setUniform("SpiritPositions", spiritPositions)

		spirits.foreach(spiritEnt => {
			val spirit = spiritEnt[Spirit]

			//			bobble = GLFW.glfwGetTime

			bobbleTime = GLFW.glfwGetTime()
			val bobble = Prelude.cosf(bobbleTime * 2.0f)

//			canvas.createQuadBuilder()
//				.withPosition(spirit.position)
//				.withColor(Vec4f.One)
//				.withNormal(spirit.position.asCartesian.normalizeSafe)
//				.withDimensions(64.0f / 56.0f * 0.25f, 0.25f)
//				.withTexture("tyche/spirits/ocean_body.png")
//				.withOrientation(Standing)
//				.draw()


			canvas.createQuadBuilder()
				.withPosition(spirit.position.plusZ(bobble * 0.015f + 0.05f).rotated(0.0f,0.001f))
				.withColor(Vec4f.One)
				.withNormal(spirit.position.asCartesian.normalizeSafe)
				.withDimensions(64.0f / 56.0f * 0.25f, 0.25f)
				.withTexture("tyche/spirits/ocean_formless.png")
   			.withOrientation(Standing)
   			.withLighting(Vec4f.One, 0.8f)
   			.draw()


			for (action <- spirit.intendedAction) {
				if (spirit.canPerformAction(world, action)) {
					val particles = graphics[Particles]
					val existing = particles.withSource(action)
					if (existing.isEmpty || existing.maxBy(p => p.startTime.inSeconds).startTime < ge.currentTime() - 0.5.seconds) {
						val randPos = new Random()

						val offsetX = randPos.nextFloat() * 0.2f - 0.1f
						val offsetY = randPos.nextFloat() * 0.2f - 0.1f
						val offsetZ = randPos.nextFloat() * 0.2f - 0.1f

						var offsetN = randPos.nextFloat() * 0.0f - 0.0f

						val gc = GlobeCoord.fromPointOnSphere(spirit.position.asCartesian + Vec3f(offsetX, offsetY, offsetZ)).withZ(1.0f + offsetN)

						val p = Particle(
							gc.interpolate(gc.plusZ(0.15f)),
							Vec2f(0.025f, 0.025f),
							Interpolation.between(spirit.color * Vec4f(1.0f,1.0f,1.0f,0.0f), spirit.color).sin010,
							"tyche/effects/particle0.png",
							5.seconds,
							1.0f
						)
						particles.addParticle(action, p, ge.currentTime())
					}
				}
			}
		})
	}
}
