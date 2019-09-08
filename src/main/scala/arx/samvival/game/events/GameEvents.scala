package arx.samvival.game.events

import arx.core.vec.coordinates.{AxialVec, AxialVec3}
import arx.engine.event.GameEvent
import arx.engine.lworld.LEntity
import arx.samvival.game.actions.GameAction
import arx.samvival.game.entities._

object GameEvents {
	case class EntityCreated(entity : LEntity) extends GameEvent

	case class EntityPlaced(entity : LEntity, at : AxialVec3) extends GameEvent

	case class EntityMoved(entity : LEntity, from : AxialVec3, to : AxialVec3) extends GameEvent

	case class FactionTurnStarted(faction : LEntity) extends GameEvent

	case class CharacterTurnStarted(character : LEntity) extends GameEvent

	case class FactionTurnEnded(faction : LEntity) extends GameEvent

	case class TurnStarted(turnNumber : Int) extends GameEvent

	case class TurnEnded(turnNumber : Int) extends GameEvent

	case class ActionTaken(action : GameAction) extends GameEvent

	case class Attack(attack : AttackProspect, result : Option[AttackResult]) extends GameEvent

	case class Strike(attacker : LEntity, defender : LEntity, strike : StrikeProspect, result : Option[StrikeResult]) extends GameEvent

	case class EquipItem(entity : LEntity, equipped : LEntity) extends GameEvent

	case class DamageTaken(character : LEntity, damage : DamageResult) extends GameEvent

	case class Died(character : LEntity) extends GameEvent

	case object TerrainGenerated extends GameEvent
}
