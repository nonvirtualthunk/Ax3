package arx.rog2.game.data.entity

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.units.UnitOfTime
import arx.core.vec.Vec3f
import arx.core.vec.Vec4f
import arx.engine.data.TGameEntityAuxData
import arx.graphics.helpers.HSBA
import arx.modules.lighting.OctantShadowGrid

import scalaxy.loops._

class LightSource extends TGameEntityAuxData {
	var lightStrength = 30.voxels
	var lightBrightness = 0.8f
	var lightColor = Vec3f(1.0f,1.0f,1.0f)
	var color = HSBA(0.0f,0.0f,1.0f,1.0f)
	var lastCalculated = none[UnitOfTime]
}
