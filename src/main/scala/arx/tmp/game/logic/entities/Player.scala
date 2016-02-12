package arx.tmp.game.logic.entities

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 4/13/13
 * Time: 12:44 PM
 * Created by nonvirtualthunk
 */

import arx.core.traits.TSentinel
import arx.tmp.game.logic.ai.TPlayerAI
import arx.tmp.game.logic.entities.core.GameEntity

@SerialVersionUID(1L)
class Player extends GameEntity {
	var number = -1
	var ai : Option[TPlayerAI] = None
	def human = ai.isEmpty

	var controlledEntities : Set[GameEntity] = Set()
}

object Player {
	val Sentinel : Player = new Player with TSentinel {
		number = -1
		override def human = false
		protected def readResolve : Object = Player.Sentinel
	}
}