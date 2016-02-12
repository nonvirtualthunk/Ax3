package arx.tmp.game.logic.command

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/19/13
 * Time: 1:33 PM
 * Created by nonvirtualthunk
 */

import arx.application.Noto
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.event.GameEventData
import arx.tmp.game.logic.event.TGameEvent

/**
 * A GameCommand represents a single discreet command given to some in-game entity. This can be triggered by a user
 * event, an AI, a macro recording, etc. The goal here is to provide a layer of indirection between the "what should
 * be done" and "how should it be done". This has general benefits, but is particularly advantageous for the AI, allowing
 * it to follow the same code paths that a human user will use.
 *
 * Simple examples of what a game command might be:
 * - MoveToCommand : indicates that the given entity should move to the provided location
 * - AttackCommand : indicates that the given entity should attack the specified target
 * - UseAbilityCommand : indicates that the given entity should use a certain special ability
 *
 *	In essence, these are the equivalent of Goal's in a direct control environment. In contrast to the previous
 *	ai/goal/planner/performer setup, commands are intended to be capable of self-executing if desired. The default
 *	implementation should be capable of using a registry of classes that indicate the ability to perform commands,
 *	but that can be overridden with a direct implementation if desired.
 */
abstract class GameCommand ( var entity : GameEntity ) extends TCommand {
	/**
	 * @return <code>true</code> if no problems were encountered, the command either finished successfully
	 *         or may be continued later, <code>false</code> if some sort of issue arose preventing this
	 *         command from being completed now, or in the future.
	 */
	def performCommand () : Boolean = {
		Noto.warn("indirect command execution is not yet supported")
		true
	}

	protected def world = entity.containingWorld

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
	protected def declareIntention ( event : TGameEvent ) : Boolean = {
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
	protected def eventOccurred ( event : TGameEvent ) : Boolean = {
		world.auxData[GameEventData].addEvent(event)
	}
}

object GameCommand {
	val Sentinel : GameCommand = new GameCommand( GameEntity.Sentinel ) {
		def isValid = false
		def isFinished = false
	}
}