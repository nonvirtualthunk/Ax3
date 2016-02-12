package arx.tmp.game.logic.entities.data

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/19/13
 * Time: 1:46 PM
 * Created by nonvirtualthunk
 */

import arx.tmp.game.logic.command.GameCommand

/**
 * Originally used in the Hellir project. Commands do not automatically end up here, they
 * are only included if the giveCommand(...) method is used here.
 */
class CommandData extends TGameEntityAuxData {
	var commands = List[GameCommand]()



	def giveCommand ( command : GameCommand ) {
		commands ::= command
	}
	def cleanUp () { commands = commands.filter( c => ! c.isFinished && c.isValid ) }
}