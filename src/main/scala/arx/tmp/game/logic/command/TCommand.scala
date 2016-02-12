package arx.tmp.game.logic.command

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/8/15
 * Time: 7:39 AM
 */

trait TCommand {
	var explanation : String = ""


	def isValid : Boolean
	def isFinished : Boolean


	def withExplanation ( str : String ) = {
		explanation = str
		this
	}
}
