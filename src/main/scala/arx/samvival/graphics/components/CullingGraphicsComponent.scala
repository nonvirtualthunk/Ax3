package arx.samvival.graphics.components

import arx.core.datastructures.Watcher
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.AxialVec
import arx.core.vec.{Vec2f, Vec3f}
import arx.engine.EngineCore
import arx.engine.advanced.lenginecomponents.LGraphicsComponent
import arx.engine.advanced.lenginepieces.LGraphicsEngine
import arx.engine.graphics.data.PovData
import arx.graphics.GL
import arx.graphics.helpers.HSBA
import arx.samvival.game.entities.Tile
import arx.samvival.graphics.data.CullingData
import scalaxy.loops.rangeExtensions

import scala.language.postfixOps

class CullingGraphicsComponent(engine : LGraphicsEngine) extends LGraphicsComponent(engine) with SamvivalGraphicsComponent {
	override def draw(): Unit = {}

	val margin = -150

	var lastCameraPos = AxialVec(-100000,-10000)
	var cameraPosWatcher = Watcher(AxialVec.fromCartesian(pov.resolve().eye.xy, SamvivalGraphicsComponent.HexSize))
	var viewportWatcher = Watcher(getCullData.mainDrawAreaFunc(GL.viewport))
	var first = true

	def getCullData = graphicsEngine.graphicsWorld[CullingData]

	override protected def updateSelf(dt: UnitOfTime): Unit = {
		val pov = graphicsEngine.graphicsWorld[PovData].pov

		if (first || cameraPosWatcher.hasChanged || viewportWatcher.hasChanged) {
			first = false
			val cullData = getCullData

			val viewport = cullData.mainDrawAreaFunc(GL.viewport)

			val centerHex = pixelToHex(Vec2f(viewport.x + viewport.width / 2, viewport.y + viewport.height / 2) / EngineCore.pixelScaleFactor, viewport)

			var inViewHexes = Set[AxialVec]()
			for (radius <- 0 until 20 optimized) {
				val iter = HexRingIterator(centerHex, radius)
				while (iter.hasNext) {
					val hex = iter.next()
					val pixel = hexToPixel(hex, viewport)

					if (pixel.x > viewport.x + margin && pixel.x < viewport.maxX - margin && pixel.y > viewport.minY + margin && pixel.y < viewport.maxY - margin) {
						inViewHexes += hex
					}
				}
			}

			cullData.hexesInView = inViewHexes
			cullData.hexesByCartesianCoord = inViewHexes.toList.sortBy(v => v.cartesianY() + v.cartesianX() * 0.0001f)
			cullData.cameraCenter = cameraPosWatcher.peek()
			cullData.currentViewport = viewportWatcher.peek()
			cullData.revision += 1
		}
	}
}
