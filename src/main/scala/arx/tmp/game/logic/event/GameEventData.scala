package arx.tmp.game.logic.event

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 6/26/12
 * Time: 5:21 PM
 * Created by nonvirtualthunk
 */

import arx.engine.data.TWorldAuxData

class GameEventData extends TWorldAuxData with Serializable {
	var _events = Vector[TGameEvent]()
	def events = _events

	def eventCount = events.size

	/**
	 * Declare that the given event will occur, unless something happens to prevent it. This is intended to allow
	 * for reactive actions, which might invalidate an event. This will only trigger listeners, and will not add
	 * the event to the list of events that have occurred. Conceptually, this should be called before any world
	 * modifications have been made
	 * @param e event that will occur unless prevented by some third party
	 * @return true if any of the pre listeners indicated that they responded to the event in some way, false otherwise
	 */
	def preEvent ( e : TGameEvent ) : Boolean = {
		preListeners.exists( f => f(e) )
	}

	/**
	 * Adds the given event to the history of events that have occurred, as well as noting it down with the subjects
	 * and objects of the event. Calls the post event listeners. Conceptually, this should only be called on events
	 * that have already occurred.
	 * @param e the event
	 * @return true if any of the post listeners indicated that they responded to the event, false otherwise
	 */
	def addEvent ( e : TGameEvent ) : Boolean = {
		_events = _events :+ e

		e.subject match {
			case Some(sub) => sub.subjectOfEvents ::= e
			case _ =>
		}

		e.objects.foreach ( _.objectOfEvents ::= e )

		e.clockPoint = _events.size - 1

		val ret = postListeners.exists( f => f(e) )

		e.consequentEvents.foreach( addEvent )

		ret
	}

	def onEvent (f : PartialFunction[TGameEvent,Boolean]): Unit = {
		postListeners ::= ((event : TGameEvent) => {
			if (f.isDefinedAt(event)) {
				f(event)
			} else {
				false
			}
		})
	}

	/**
	 * Listening functions which are to be given each event before it occurs. This results in the possibility that
	 * one of these functions will modify the world in such a way that the event can no longer occur, and that must
	 * be taken into account at other layers of the game engine. This is an unfortunate but necessary side effect of
	 * allowing for response actions (firing a weapon in response to a character moving, etc)
	 */
	var preListeners : List[ (TGameEvent) => Boolean ] = Nil

	/**
	 * Listening functions which are to be given each event after it occurs. Since conceptually, these will be called
	 * after the event has already occurred, they cannot modify the world to make those events invalid without themselves
	 * performing invalid operations. As such, less care needs to be taken than with the preListeners
	 */
	var postListeners : List[ (TGameEvent) => Boolean ] = Nil
}