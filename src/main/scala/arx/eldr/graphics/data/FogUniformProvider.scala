package arx.eldr.graphics.data

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.core.Moddable
import arx.core.vec.Vec3f
import arx.eldr.game.world.data.Light
import arx.engine.world.World
import arx.graphics.pov.TCamera
import arx.graphics.shader.Shader
import scalaxy.loops._

class FogUniformProvider(env : World, pov : Moddable[TCamera]) {
	def lowColor = env[Light].lowSkyColor
	def highColor = env[Light].highSkyColor
	val fogStartPcnt = 0.65f

	def setUniforms (shader:Shader){
		setFogVariables(shader)
		setOffsetRelativeToEye(shader,pov.eye * -1.0f)
	}
	def setFogVariables(shader: Shader) {
		val fogEnd = pov.viewDistance - 2.0f
		val fogStart = fogEnd * fogStartPcnt

		shader.setUniform("lowColor",lowColor,tolerateAbsence = true)
		shader.setUniform("highColor",highColor,tolerateAbsence = true)
		shader.setUniform("fogEnd",fogEnd,tolerateAbsence = true)
		shader.setUniform("oneOverFogRange",1.0f/(fogEnd - fogStart),tolerateAbsence = true)
	}
	def setOffsetRelativeToEye ( shader : Shader , offsetRelativeToEye: Vec3f ) {
		shader.setUniform("vertexOriginOffset",offsetRelativeToEye,tolerateAbsence = true)
	}
}
