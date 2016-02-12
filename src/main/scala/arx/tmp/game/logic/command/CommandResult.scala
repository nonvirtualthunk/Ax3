package arx.tmp.game.logic.command

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/19/13
 * Time: 1:54 PM
 * Created by nonvirtualthunk
 */

object CommandResult extends Enumeration {
	type CommandResult = Value
	val Success, Failure, Unfinished = Value
}