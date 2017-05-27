package arx.rog2.game

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.control.event.Event.Event
import arx.engine.entity.TGameEntity

import scalaxy.loops._

case class EntityMovedEvent (entity : TGameEntity, from : VoxelCoord, to : VoxelCoord) extends Event {

}

case class EntityAttackedEvent (entity : TGameEntity, target : TGameEntity, damageDealt : Int) extends Event
