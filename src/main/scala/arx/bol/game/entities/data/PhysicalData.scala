package arx.bol.game.entities.data

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.vec.Vec2f
import arx.engine.data.TGameEntityAuxData

import scalaxy.loops._

class PhysicalData extends TGameEntityAuxData {
	var location = Vec2f.Zero
	var size = Vec2f.One
}
