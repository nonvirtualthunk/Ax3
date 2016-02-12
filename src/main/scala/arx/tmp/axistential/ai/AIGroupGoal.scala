package arx.axistential.ai

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/9/15
 * Time: 8:05 AM
 */

import arx.Prelude._
import arx.core.units.UnitOfTime

trait AIGroupGoal {
	 /** Create a new goal to be executed by the colony, if necessary, or None otherwise */
	 def createSubGoal(forGroup : TAIGroup) : Option[Goal]

	 /** How often this should be checked for new work that needs to be done */
	 def checkInterval : UnitOfTime

	 var lastChecked = 0.seconds
	 var activeSubGoal : Option[Goal] = None
 }
