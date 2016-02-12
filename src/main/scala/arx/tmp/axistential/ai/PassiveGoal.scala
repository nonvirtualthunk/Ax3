package arx.axistential.ai

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/31/14
 * Time: 1:00 PM
 */

trait PassiveGoal extends Goal {
	def fitness ( agent : TAIAgent ) = 0
	def progressRequired ( agent : TAIAgent ) = Float.MaxValue
}
