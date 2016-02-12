package arx.tmp.game.logic.event

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 6/26/12
 * Time: 5:21 PM
 * Created by nonvirtualthunk
 */

import arx.core.units.UnitOfTime
import arx.engine.game.GameEngine
import arx.core.vec.coordinates.TMajorCoord
import arx.tmp.game.logic.entities.core.GameEntity

trait TGameEvent extends Event {
	var name : String = "event"
	val timestamp : UnitOfTime = GameEngine.time
	var clockPoint = 0

	var _consequentEvents : List[TGameEvent] = Nil
	def consequentEvents = _consequentEvents
	var _consequenceOf : Option[TGameEvent] = None
	def consequenceOf = _consequenceOf
	def consequenceOf_= ( event : TGameEvent ) {
		_consequenceOf = Some(event)
		event._consequentEvents ::= this
	}

	var location : Option[TMajorCoord] = None

	def subject : Option[GameEntity] = None
	def objects : List[GameEntity] = Nil
}