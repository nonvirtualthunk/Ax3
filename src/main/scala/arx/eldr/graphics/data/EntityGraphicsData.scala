package arx.eldr.graphics.data

/**
  * Storage for purely graphical information about an entity
  */

import arx.Prelude._
import arx.core.Moddable
import arx.core.vec.{ReadVec3f, Vec3f}
import arx.core.vec.coordinates.ObjectCoord
import arx.eldr.game.entity.data.PhysicalData
import arx.engine.data.TGameEntityAuxData
import arx.engine.entity.TGameEntity

import scalaxy.loops._


// TODO: mark as non-serialized
class EntityGraphicsData extends TGameEntityAuxData {
	var graphicalPosition : Moddable[ObjectCoord] = ObjectCoord.Sentinel
	var facing : Moddable[ReadVec3f] = Vec3f.UnitX
}
