package arx.rog2.graphics

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 6/4/17
  * Time: 6:37 PM
  */

import arx.Prelude._
import arx.core.datastructures.Watcher

import scalaxy.loops._
import arx.core.vec._
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.graphics.GraphicsEngine
import arx.rog2.graphics.data.{Overlay, OverlayRegion}

class OverlayGraphicsComponent(eng: GraphicsEngine) extends RogGraphicsComponent(eng) {
	lazy val watcher = Watcher(graphics[Overlay].overlaidRegions)

	override def needsUpdate: Boolean = {
		super.needsUpdate || watcher.hasChanged
	}

	override def draw(canvas: RogCanvas): Unit = {
		val overlayData = graphics[Overlay]

		val lav = new LightAndVisionContext

		overlayData.overlaidRegions.values.foreach {
			case OverlayRegion(region, image, color) =>
				for (vox <- region.edgeVoxels) {
					for (q <- 0 until 6 optimized) {
						val adj = vox + Cardinals.dirvec(q)
						if (!region.contains(adj)) {
							canvas.quad(vox - VoxelCoord.Center)
								.withCubeFace(q)
								.withTexture(image)
								.withColor(color)
								.withVisionPcnt(lav.visionPcntAt(vox,q))
								.withLightColor(Vec3f.One)
								.draw()
						}
					}
				}
		}
	}
}
