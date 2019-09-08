package arx.tyche.graphics.components

import arx.core.vec.Vec4f
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.GraphicsComponent
import arx.resource.ResourceManager
import arx.tyche.core.GlobeCoord
import arx.tyche.game.data.{Globe, Patch, SourceType}
import arx.tyche.graphics.components.TyOrientation.Standing
import org.joml.Quaternionf
import org.lwjgl.opengl.GL11

import scala.util.Random

class PatchSourceGraphics(ge : GraphicsEngine) extends TyGraphicsComponent(ge) {

	canvas.textureBlock.magFilter = GL11.GL_NEAREST

	override def draw(canvas: TyCanvas): Unit = {
		val globe = world[Globe]
		globe.patches.foreach { patch => {
			val patchData = patch[Patch]

			if (patchData.sourceType != SourceType.None) {
				val imgFrame = (ge.currentTime().inMilliseconds.toInt / 500) % patchData.sourceType.animationFrames
				val img = ResourceManager.image(s"tyche/sources/${patchData.sourceType.name}/anim${imgFrame + 1}.png")

				val rand = new Random(patchData.center.hashCode())

				for (i <- 0 until 5) {
					val theta = (rand.nextFloat() - 0.5f) * Math.PI * 2.0f * 0.02f
					val phi = (rand.nextFloat() - 0.5f) * Math.PI * 2.0f * 0.02f

//					val newRot = patchData.center.rotation.rotateLocalY(theta.toFloat, new Quaternionf()).rotateLocalZ(phi.toFloat)
					val newRot = patchData.center.rotation.rotateY(theta.toFloat, new Quaternionf()).rotateZ(phi.toFloat)
					val pos = patchData.center.withRotation(newRot)

					canvas.quad(pos)
						.withOrientation(Standing)
						.withColor(Vec4f.One)
						.withTexture(img)
						.withNormal(pos.asCartesian.normalizeSafe)
						.withDimensions(0.035f, 0.035f)
						.draw()
				}
			}
		}}
	}


}
