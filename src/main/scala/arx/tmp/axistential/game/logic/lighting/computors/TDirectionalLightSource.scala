package nowiamthefastest.game.logic.components.computors

import arx.core.vec.Vec3f
import arx.tmp.game.logic.entities.TLightSource

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 12/13/11
 * Time: 9:00 PM
 * Created by nonvirtualthunk
 */



trait TDirectionalLightSource extends TLightSource{
	var _lightVector = Vec3f(1.0f,0.0f,0.0f)
	var _attenuation = 4

	def lightVector = _lightVector
	def attenuation = _attenuation
	override def directional = true
}