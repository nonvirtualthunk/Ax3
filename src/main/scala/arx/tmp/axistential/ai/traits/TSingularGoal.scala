package arx.axistential.ai.traits

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/4/15
 * Time: 11:19 AM
 */

import arx.axistential.ai.Goal
import arx.axistential.ai.SplitResult
import arx.axistential.ai.TAIAgent

trait TSingularGoal extends Goal {
	/** If this goal can be split into more than one part, create two new goals, the first representing
	  * the split off portion, the second representing the remainder
	  * @param agent the agent who is to perform the split off section
	  */
	override def split(agent: TAIAgent): SplitResult = this
}
