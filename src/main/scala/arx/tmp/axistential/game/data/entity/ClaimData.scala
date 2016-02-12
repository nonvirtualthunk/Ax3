package arx.axistential.game.data.entity

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/16/13
 * Time: 3:58 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.application.Noto
import arx.axistential.ai.Goal
import arx.core.representation.InformationLevel
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.core.TGameBase
import arx.tmp.game.logic.entities.data.TGameEntityAuxData

class ClaimData extends TGameEntityAuxData {
	protected var _claimedBy : Option[GameEntity] = None
	protected var _claimedFor : Option[Goal] = None
	protected var _forEntity : GameEntity = null

	def claimedBy = _claimedBy
	def claimedFor = _claimedFor
	def isClaimed = _claimedBy.nonEmpty
	final def notClaimed = ! isClaimed


	override def onAssignedToEntity(entity: TGameBase): Unit = _forEntity = entity match { case ge : GameEntity => ge ; case _ => GameEntity.Sentinel }

	def claim ( by : GameEntity , forGoal : Goal ) {
		if ( _claimedBy.isEmpty ) {
			_claimedBy = Some(by)
			_claimedFor = Some(forGoal)
			forGoal.claimedEntities ::= _forEntity

		}
	}

	def unclaim ( by : GameEntity , forGoal : Goal ) {
		if ( _claimedBy != Some(by) ) {
			Noto.warn(s"unclaim called, but claim data was not in expected state, claimed by ${_claimedBy}, for ${_claimedFor}")
		}
		_claimedBy = None
		_claimedFor = None
	}

	/**
	 * Method for retrieving key-value pairs representing the data contained in this object, intended for
	 * ui use.
	 *
	 * @param level how fine the detail should be, can be used to determine what is returned (i.e. metadata is
	 *              only returned on Fine or higher levels)
	 * @return key-value pairs
	 */
	override def informationPairs(level: InformationLevel.InformationLevel): Map[String, Any] = Map(
		"claimed by" -> claimedBy,
		"claimed for" -> claimedFor
	)
}

object NoClaimData extends ClaimData {
	override def claimedBy = None
	override def claimedFor = None
	override def isClaimed = false
}