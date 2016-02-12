package arx.axistential.game.data.entity

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/10/13
 * Time: 11:40 AM
 * To change this template use File | Settings | File Templates.
 */

import arx.core.Moddable
import arx.core.traits.TSentinel
import arx.core.vec.ReadVec4f
import arx.core.vec.Vec4f
import arx.tmp.game.logic.entities.data.TGameEntityAuxData

class CosmeticData extends TGameEntityAuxData {
	var _color = Moddable(Vec4f.One)
	def color = _color ; def color_=(c:Moddable[ReadVec4f]) { _color = c }

//	var _state = "default"
//	def state = _state; def state_= ( s : String ) { _state = s }


}

object CosmeticData {
	val Sentinel : CosmeticData = new CosmeticData with TSentinel {
		val _rawcolor = Moddable(Vec4f.One)
		override def color = _rawcolor

//		override def state = "default"
	}
}