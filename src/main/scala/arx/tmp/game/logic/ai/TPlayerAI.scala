package arx.tmp.game.logic.ai

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/19/13
 * Time: 1:32 PM
 * Created by nonvirtualthunk
 */

import arx.tmp.game.logic.command.GameCommand
import arx.tmp.game.logic.entities.Player

trait TPlayerAI {
	def computeCommand ( player : Player, world : World ) : Option[GameCommand]
	def commandFailed ( command : GameCommand )
}