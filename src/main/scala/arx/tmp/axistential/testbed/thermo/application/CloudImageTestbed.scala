package arx.axistential.testbed.thermo.application

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/27/13
 * Time: 3:31 PM
 */

import arx.core.units.UnitOfTime
import arx.core.vec.Vec2f
import arx.core.vec.Vec4f
import arx.tmp.game.procedural.generators.CloudImageGenerator
import arx.tmp.game.procedural.generators.ImageTransformations
import arx.graphics.Image

class CloudImageTestbed extends DeadSimpleConflux {
	val img = CloudImageGenerator.generate(256,256,1.0f,3)
	ImageTransformations.rescaleBrightness(img,0.5f,1.0f)
//	img.setPixelsFromFunc((x,y) => Vec4i(0,0,0,255))

	val checkerboard = Image.withDimensions(128,128).setPixelsFromFunc( (x,y,rgba) => {
		rgba match {
			case 3 => 255
			case _ => if ( x%2 == y%2 ) { 200 } else { 100 }
		}
	})

	def draw(graphics: GraphicsHelper): Unit = {
		graphics.drawQuad(Vec2f.Zero,Vec2f(15.0f),checkerboard,Vec4f.One)
		graphics.drawQuad(Vec2f.Zero,Vec2f(15.0f),img,Vec4f.One)
	}

	def onKeyPress(kpe: KeyPressEvent): Unit = {}

	def onKeyRelease(kpe: KeyReleaseEvent): Unit = {}

	def onMousePress(mpe: MousePressEvent): Unit = {}

	def onMouseRelease(mre: MouseReleaseEvent): Unit = {}

	def onMouseMove(mme: MouseMoveEvent): Unit = {}

	def onMouseDrag(mde: MouseDragEvent): Unit = {}

	def tick(timeElapsed: UnitOfTime): Unit = {}
}
