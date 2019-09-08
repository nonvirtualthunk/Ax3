package arx.tyche.graphics.components

import arx.core.vec.Vec4f
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.DrawPriority
import arx.tyche.graphics.components.TyOrientation.Billboard
import arx.tyche.graphics.data.Particles

class ParticleGraphics(ge: GraphicsEngine) extends TyGraphicsComponent(ge) {
	drawOrder = DrawPriority.Late

	override def draw(canvas: TyCanvas): Unit = {
		val curTime = ge.currentTime()

		for ((src, particles) <- graphics[Particles].particlesBySource) {
			particles.foreach(p => {
				canvas.quad(p.position(curTime))
					.withTexture(p.texture)
					.withDimensions(p.dimensions(curTime))
					.withColor(p.color(curTime))
					.withNormal(p.position(curTime).asCartesian.normalizeSafe)
					.withOrientation(Billboard)
					.withLighting(Vec4f.One, p.lightPcnt(curTime))
					.draw()
			})
		}

		graphics[Particles].cullParticles(curTime)
	}
}
