package arx.samvival.graphics.components

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/18/18
  * Time: 8:35 AM
  */

import arx.Prelude._
import scalaxy.loops._
import arx.core.vec._
import arx.core.vec.coordinates.{CartVec, CartVec3}
import arx.engine.advanced.lenginepieces.LGraphicsEngine
import arx.graphics.helpers.HSBA

class SamvivalTestGraphicsComponent(engine : LGraphicsEngine) extends SamvivalCanvasGraphicsComponent(engine) {
	override def draw(canvas: SVCanvas): Unit = {
	canvas.quad(CartVec3(0.0f,0.0f,0.0f), 0)
   		.withColor(HSBA(0.0f,0.0f,1.0f,1.0f))
   		.withTexture("samvival/terrain/grass.png")
   		.withDimensions(Vec2f(142.0f,124.0f))
   		.withLightColor(Vec3f.One)
   		.draw()
	}
}
