package arx.axistential.ai.traits

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 4/17/15
 * Time: 7:32 AM
 */

import arx.axistential.ai.Goal
import arx.axistential.ai.TAIAgent

/** A Goal that may need semi-continuous recalculation in response to a changing
  * environment, without a Thinking pause. The primary example at this time would
  * be chasing another entity. Using the standard Retry mechanism results in the
  * creature pausing every few steps, which is very off-putting.
  *
  * If <code>needsRecalculation()</code> returns true, the createPrerequisiteGoals(...)
  * and plan(...) functions will be re-called and used to replcae the existing values.
  */
trait TRecalculatingGoal extends Goal {
	/** Should return true if a recalculation is needed at this time, false otherwise */
	def needsRecalculation(agent : TAIAgent) : Boolean

	/** Should return a top level copy of this goal. Should not copy internals, planned
	  * information, progress, etc. It should essentially supply matching constructor args
	  * and nothing else. */
	def createTopLevelCopy : TRecalculatingGoal

	var recalculationRequested = false
}
