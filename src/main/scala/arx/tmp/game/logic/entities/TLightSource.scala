package arx.tmp.game.logic.entities

import arx.core.vec.Vec4f
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.entities.core.TGameEntityTrait
import arx.tmp.game.logic.event.TGameEvent

/**
 *
 */

trait TLightSource extends TGameEntityTrait {
	var _lightStrength : Byte = 30.toByte
	var _lightColor : Vec4f = Vec4f(1.0f,1.0f,1.0f,1.0f)
	@transient var lightChannelIndex = -1

	def motile = false
	def directional = false
	def lightActive = true

	def lightStrength : Byte = _lightStrength
	def lightStrength_= ( b : Byte ) {
		val old = _lightStrength
		_lightStrength = b
		if ( old != b ) {
			this.eventHappened( LightStrengthChanged(old,b) )
		}
	}
	def lightColor : Vec4f = _lightColor
	def lightColor_= ( v : Vec4f ) {
		val old = _lightColor
		_lightColor = v
		if ( old != v ) {
			this.eventHappened( LightColorChanged(old,v) )
		}
	}
	def variableColor : Boolean = false

	def lightLocation : VoxelCoord
	def exactLightLocation : ObjectCoord = lightLocation.toObjectCoord
}



case class LightPositionChanged ( curVoxel : VoxelCoord , newVoxel : VoxelCoord ) extends TGameEvent
case class LightStrengthChanged ( oldStrength : Byte , newStrength : Byte ) extends TGameEvent
case class LightColorChanged ( oldColor : Vec4f , newColor : Vec4f ) extends TGameEvent