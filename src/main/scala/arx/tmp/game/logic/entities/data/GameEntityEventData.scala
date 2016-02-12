package arx.tmp.game.logic.entities.data

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/8/13
 * Time: 8:21 AM
 * Created by nonvirtualthunk
 */

import arx.tmp.game.logic.event.TGameEvent

class GameEntityEventData extends TGameEntityAuxData {
	var _subjectOfEvents : List[TGameEvent] = Nil
	var _objectOfEvents : List[TGameEvent] = Nil

	def subjectOfEvents = _subjectOfEvents
	def subjectOfEvents_=(l:List[TGameEvent]) {_subjectOfEvents = l}

	def objectOfEvents = _objectOfEvents
	def objectOfEvents_=(l:List[TGameEvent]) {_objectOfEvents = l}
}

object SentinelGameEntityEventData extends GameEntityEventData {
	override def subjectOfEvents = Nil
	override def subjectOfEvents_=(l:List[TGameEvent]) {}

	override def objectOfEvents = Nil
	override def objectOfEvents_=(l:List[TGameEvent]) {}
}