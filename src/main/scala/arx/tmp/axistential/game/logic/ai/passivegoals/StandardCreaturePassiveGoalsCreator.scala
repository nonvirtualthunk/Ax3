package arx.axistential.game.logic.ai.passivegoals

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/31/14
 * Time: 1:04 PM
 */

import arx.axistential.ai.PassiveGoal
import arx.axistential.ai.PassiveGoalCreator
import arx.axistential.ai.TAIAgent
import arx.axistential.game.entities.CreatureEntity
import arx.axistential.game.logic.ai.goals.EmptyInventoryGoal
import arx.axistential.modules.hunger.EatWhenHungry

object StandardCreaturePassiveGoalsCreator extends PassiveGoalCreator {
	override def createPassiveGoalsFor(agent: TAIAgent): List[PassiveGoal] = {
		agent match {
			case ce : CreatureEntity => {
				List(
					new EatWhenHungry
					,new FleeFromEnemies
					,new MoveOutOfBadLocationGoal
					,new EmptyInventoryGoal
				)
			}
			case _ => Nil
		}
	}
}