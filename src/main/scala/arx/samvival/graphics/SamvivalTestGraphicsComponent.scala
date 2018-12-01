package arx.samvival.graphics

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/18/18
  * Time: 8:35 AM
  */

import arx.Prelude._
import scalaxy.loops._
import arx.core.vec._
import arx.engine.advanced.lenginepieces.LGraphicsEngine
import arx.graphics.helpers.HSBA

class SamvivalTestGraphicsComponent(engine : LGraphicsEngine) extends SamvivalGraphicsComponent(engine) {
	override def draw(canvas: SVCanvas): Unit = {
		canvas.quad(Vec2f(0.0f,0.0f))
   		.withColor(HSBA(0.3f,0.5f,0.5f,1.0f))
   		.withTexture("default/blank.png")
   		.withDimensions(Vec2f(10.0f,10.0f))
   		.draw()
	}
}
