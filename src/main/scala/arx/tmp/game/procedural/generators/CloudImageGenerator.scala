package arx.tmp.game.procedural.generators

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/2/13
 * Time: 2:02 PM
 */

import arx.Prelude._
import arx.core.gen.SimplexNoise
import arx.graphics.Image

object CloudImageGenerator {
	def generate ( width : Int , height : Int, turbulence : Float = 1.0f , bandingShift : Int = 0 ) = {
		val s = width / 256.0f

		val img = Image.withDimensions(width,height)
		import arx.core.gen.ArxGenerators._
		val noise = new SimplexNoise(System.currentTimeMillis())
		val gen = Turbulence( Scale(0.05f / s) >> Fractal(2)(Simplex(noise)) >> Mult(2 * s * turbulence) ) >>
			Scale(0.0075f / s) >> Fractal(4)(Simplex(noise)) >> Mult(0.45f) >> Add(0.5f) >> Clamp(0.0f,1.0f)
		val dist = (x:Int,y:Int) => (1.0f - powf(distance(x,y,img.width/2,img.height/2) / (img.width.toFloat * 0.5f),2.0f)).clamp(0.0f,1.0f)

		img.setPixelsFromFunc((x,y,rgba) => {
			val p = gen(x,y) - (1.0f - dist(x,y))
			val v_base = (p * 255).toInt.clamp(0,255)
			val v = (v_base >> bandingShift) << bandingShift

			rgba match {
				case 3 => if ( v > 50 ) { 255 } else if ( v > 0 ) { ((v / 50.0f) * 255).toInt } else { 0 }
				case _ => {
					255 - v
				}
			}
		})

		img
	}
}
