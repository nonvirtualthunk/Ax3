package arx.tyche.graphics.components

import arx.core.vec.{Vec3f, Vec4f}
import arx.engine.graphics.GraphicsEngine
import arx.tyche.core.GlobeCoord
import arx.tyche.game.data.Sun
import arx.tyche.graphics.components.TyOrientation.Billboard
import org.lwjgl.opengl.GL11

class SunGraphics(ge : GraphicsEngine) extends TyGraphicsComponent(ge) {


	canvas.textureBlock.magFilter = GL11.GL_NEAREST
	canvas.textureBlock.minFilter = GL11.GL_NEAREST

	override def draw(canvas: TyCanvas): Unit = {
		val sunCoord = GlobeCoord.fromEuler(world[Sun].theta, world[Sun].phi, 50.0f)
		shader.setUniform("SunPosition", sunCoord.asCartesian)


		canvas.createQuadBuilder()
			.withOrientation(Billboard)
			.withNormal(sunCoord.asCartesian.normalizeSafe)
			.withPosition(sunCoord.withZ(7.0f))
			.withColor(Vec4f.One)
			.withTexture("tyche/world/sun.png")
			.draw()


		canvas.createQuadBuilder()
			.withOrientation(Billboard)
			.withNormal(sunCoord.asCartesian.normalizeSafe)
			.withPosition(GlobeCoord.fromPointOnSphere(sunCoord.asCartesian.normalizeSafe * -1.0f).withZ(7.0f))
			.withColor(Vec4f.One)
			.withTexture("tyche/world/moon.png")
			.draw()
	}
}
