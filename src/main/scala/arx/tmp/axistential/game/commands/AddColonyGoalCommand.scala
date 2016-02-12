package arx.axistential.game.commands

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/6/15
 * Time: 9:05 AM
 */

import arx.axistential.ai.Goal
import arx.axistential.ai.PassiveGoal
import arx.axistential.ai.TAIAgent
import arx.axistential.game.entities.groups.Colony
import arx.tmp.game.logic.event.TGameEvent

case class AddColonyGoalCommand(colony : Colony, goal : Goal) extends AnthCommand(colony) {
	override def isValid: Boolean = true
	override def isFinished: Boolean = colony.goals.contains(goal)

	/**
	 * @return <code>true</code> if no problems were encountered, the command either finished successfully
	 *         or may be continued later, <code>false</code> if some sort of issue arose preventing this
	 *         command from being completed now, or in the future.
	 */
	override def performCommand(): Boolean = {
		val event = ColonyGoalAddedEvent(colony,goal)
		declareIntention(event)
		if (isValid) {
			colony.addGoal(goal)
			eventOccurred(event)
		}
		true
	}
}

case class ColonyGoalAddedEvent (colony : Colony, goal : Goal) extends TGameEvent
case class PassiveEntityGoalAddedEvent (agent : TAIAgent, goal : PassiveGoal) extends TGameEvent

case class AddEntityGoalCommand(agent : TAIAgent, goal : PassiveGoal) extends AnthCommand(agent) {
	override def isValid: Boolean = true
	override def isFinished: Boolean = {
		agent.passiveGoals.contains(goal)
	}

	/**
	 * @return <code>true</code> if no problems were encountered, the command either finished successfully
	 *         or may be continued later, <code>false</code> if some sort of issue arose preventing this
	 *         command from being completed now, or in the future.
	 */
	override def performCommand(): Boolean = {
		val event = PassiveEntityGoalAddedEvent(agent,goal)
		declareIntention(event)
		if (isValid) {
			agent.passiveGoals ::= goal
			eventOccurred(event)
		}

		true
	}
}