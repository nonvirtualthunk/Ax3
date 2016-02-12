package arx.tmp.game.logic.command

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/8/15
 * Time: 7:37 AM
 */

import arx.tmp.game.logic.event.GameEventData
import arx.tmp.game.logic.event.TGameEvent

abstract class WorldCommand extends TCommand {
	/**
	 * @return <code>true</code> if no problems were encountered, the command either finished successfully
	 *         or may be continued later, <code>false</code> if some sort of issue arose preventing this
	 *         command from being completed now, or in the future.
	 */
	def performCommand (world : World) : Boolean

	/**
	 * Declare this commands intention to have the specified event occur. This may trigger any number of conditional
	 * effects (an archer preparing to fire when an enemy moves, a caster set to raise a wall around anyone who tries
	 * to fire, etc). The intended pattern is that one should declare first, let that trigger, then test that the
	 * world is still in a valid state for the given event to occur and if it is perform the world modifications. After
	 * that the paired eventOccurred should be called.
 *
	 * @param event the event intended to occur
	 * @return true if any effects were triggered, false otherwise
	 */
	protected def declareIntention ( world : World, event : TGameEvent ) : Boolean = {
		world.auxData[GameEventData].preEvent(event)
	}

	/**
	 * Declare that the given event has occurred. This may trigger any number of conditional effects, as is the case
	 * with declareIntention, however, since this will be after modifications have been made, the likelihood of this
	 * causing a problem is much lower.
 *
	 * @param event the event that has occurred
	 * @return true if any effects were triggered, false otherwise
	 */
	protected def eventOccurred ( world : World, event : TGameEvent ) : Boolean = {
		world.auxData[GameEventData].addEvent(event)
	}

}
