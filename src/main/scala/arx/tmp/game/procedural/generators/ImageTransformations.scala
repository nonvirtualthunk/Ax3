package arx.tmp.game.procedural.generators

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/3/13
 * Time: 3:54 PM
 */

import arx.core.vec.Vec4i
import arx.graphics.Image

object ImageTransformations {
	def rescaleBrightness ( img : Image , min : Float , max : Float ) {
		val range = max - min

		val ret = Vec4i(0,0,0,0)
		img.transformPixelsByFunc( (x,y,v) => {
			val rf = v.r / 255.0f
			val gf = v.g / 255.0f
			val bf = v.b / 255.0f

			ret.r = ((min + rf * range) * 255).toInt
			ret.g = ((min + gf * range) * 255).toInt
			ret.b = ((min + bf * range) * 255).toInt
			ret.a = v.a
			
			ret
		})
	}

//	def bandBrightness ( img : Image , numBands : Int ) {
//		var minBrightness = 255
//		var maxBrightness = 0
//		img.foreachPixel((x,y,v) => {
//			val b =
//		})
//	}
}
