package arx.samvival.game.logic

import arx.application.Noto
import arx.engine.lworld.LWorld
import arx.samvival.game.actions.{AttackAction, GameAction, MoveAction, SwitchActiveCharacter}
import arx.samvival.game.events.GameEvents.ActionTaken
import arx.samvival.graphics.data.Selection

object Action {


	def performAction(action : GameAction)(implicit world : LWorld) : Boolean = {
		world.startEvent(ActionTaken(action))
		val res = action match {
			case MoveAction(character, path) =>
				Movement.moveCharacterOnPath(character, path)
			case SwitchActiveCharacter(character) =>
				true
			case a: AttackAction =>
				Attacks.attack(a)
				true
			case _ =>
				Noto.error(s"Unsupported action: $action")
				false
		}
		world.endEvent(ActionTaken(action))
		res
	}



}
