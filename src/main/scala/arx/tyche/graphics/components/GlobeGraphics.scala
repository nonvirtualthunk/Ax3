package arx.tyche.graphics.components

import arx.Prelude
import arx.core.vec.{ReadVec4f, Vec3f, Vec4f}
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.GraphicsComponent
import arx.graphics.helpers.Color
import arx.resource.ResourceManager
import arx.tyche.core.GlobeCoord
import arx.tyche.game.data.{Globe, Patch, SourceType, Spirit, TerrainType}
import arx.tyche.graphics.components.TyOrientation.Standing
import scalaxy.loops._

import scala.util.Random

class GlobeGraphics(ge : GraphicsEngine) extends TyGraphicsComponent(ge) {
	import TerrainType._

	val colorsByTerrainType : Map[TerrainType, ReadVec4f] = Map(
		Desolation -> Color(64, 68, 74, 255),
		Grassland -> Color(105, 151, 30, 255),
		Ocean -> Color(49, 96, 173, 255)
	)

	override def draw(canvas: TyCanvas): Unit = {
		val globe = world[Globe]
		val nextRevision = globe.revision.get

		globe.patches.foreach { patch => {
			val patchData = patch[Patch]
			var color = colorsByTerrainType.getOrElse(patchData.terrainType, Vec4f(1.0f, 0.0f, 0.0f, 1.0f))
			if (patchData.terrainType == Desolation && patchData.hydration > 0) {
				color *= Vec4f(0.7f,0.7f,0.9f, 1.0f)
			}

			val numV = patchData.vertices.size
			for (j <- 0 until numV optimized) {
				val p1 = patchData.vertices(j)
				val p2 = patchData.vertices((j + 1) % numV)
				val p3 = patchData.center


				val centerNormal = p3.asCartesian.normalize
				canvas.tri(p1, p2, p3)
					.withColor(color)
					.withNormals(centerNormal, centerNormal, centerNormal)
					.draw()
			}
		}}

//		globe.delaunyTris.foreach(t => {
//			val color= Vec4f(rand.nextFloat(), rand.nextFloat(), rand.nextFloat(), 1.0f)
//			canvas.tri(t)
//   			.withColor(color)
//   			.draw()
//		})

		lastDrawnRevision = nextRevision
	}

	var lastDrawnRevision = 0
	override def needsUpdate: Boolean = {
		world[Globe].revision.get > lastDrawnRevision
//		true
	}
}

object GlobeGraphics {

}