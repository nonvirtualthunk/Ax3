package arx.rog2.game.events

/**
  * TODO: Add javadoc
  */

import arx.core.vec.coordinates.VoxelCoord
import arx.engine.control.event.Event.Event
import arx.engine.entity.TGameEntity
import arx.engine.event.GameEvent
import arx.rog2.game.actions.DamageDone
import arx.rog2.game.data.entity.BodySlot

object Rog {
	sealed trait RogLogLevel {
		def ordinal : Int
	}
	case object Fine extends RogLogLevel {
		override def ordinal = 1
	}
	case object Info extends RogLogLevel {
		override def ordinal: Int = 2
	}
	case object Error extends RogLogLevel {
		override def ordinal: Int = 4
	}

	case class LogMessage(text : String, references : List[Any], level : Rog.RogLogLevel)
}

abstract class RogGameEvent(entity : TGameEntity) extends GameEvent {
	def logMessage : Rog.LogMessage
}

case class ErrorEvent(entity : TGameEntity, error : String) extends RogGameEvent(entity) {
	override def logMessage: Rog.LogMessage = Rog.LogMessage("error encountered: " + error,Nil,Rog.Error)
}

case class EntityMovedEvent (entity : TGameEntity, from : VoxelCoord, to : VoxelCoord) extends RogGameEvent(entity) {
	override def logMessage = Rog.LogMessage("@0 moved from @1 to @2", entity :: from :: to :: Nil, Rog.Fine)
}

case class EntityAttackedEvent (entity : TGameEntity, target : TGameEntity, damageDealt : DamageDone) extends RogGameEvent(entity) {
	override def logMessage = Rog.LogMessage("@0 attacked @1 for @2 damage", entity :: target :: damageDealt :: Nil, Rog.Info)
}

case class EntityTookItemEvent (entity : TGameEntity, item : TGameEntity) extends RogGameEvent(entity) {
	override def logMessage: Rog.LogMessage = Rog.LogMessage("@0 picked up @1", entity :: item :: Nil, Rog.Info)
}

case class EntityPlacedItemEvent (entity : TGameEntity, item : TGameEntity, location : VoxelCoord) extends RogGameEvent(entity) {
	override def logMessage: Rog.LogMessage = Rog.LogMessage("@0 placed @1", entity :: item :: location :: Nil, Rog.Info)
}

case class EntityEquippedItemEvent (entity : TGameEntity, item : TGameEntity, toSlot : BodySlot) extends RogGameEvent(entity) {
	override def logMessage: Rog.LogMessage = Rog.LogMessage("@0 equipped @1 to @2", entity :: item :: toSlot :: Nil, Rog.Info)
}