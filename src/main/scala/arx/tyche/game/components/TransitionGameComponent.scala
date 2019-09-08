package arx.tyche.game.components

import arx.core.math.Interpolation
import arx.core.units.UnitOfTime
import arx.engine.data.TWorldAuxData
import arx.engine.game.GameEngine
import arx.engine.game.components.GameComponent
import arx.Prelude._

class TransitionGameComponent(ge : GameEngine) extends GameComponent(ge) {
	override protected def updateSelf(dt: UnitOfTime): Unit = {
		val curTime = ge.currentTime()
		var toRemove = Set[Transition[_]]()
		for (typedTransition <- world[Transitions].transitions) {
			// cheat around the typing, we know it will be correct because it's enforced at construction time
			val transition = typedTransition.asInstanceOf[Transition[AnyRef]]

			if (transition.startTime <= 0.seconds) {
				transition.startTime = curTime
			}

			if (transition.delay <= curTime - transition.startTime) {
				val pcnt = if (transition.duration <= 0.seconds) {
					1.0f
				} else {
					(((curTime - transition.startTime) - transition.delay).inSeconds / transition.duration.inSeconds.max(0)).clamp(0.0f, 1.0f)
				}

				transition.setter(transition.interpolation.interpolate(pcnt))

				if (pcnt >= 1.0f) {
					toRemove += transition
				}
			}
		}

		world[Transitions].transitions = world[Transitions].transitions.filterNot(toRemove.contains)
	}
}

class Transitions extends TWorldAuxData {
	@transient private var _transitions : List[Transition[_]] = Nil
	def transitions = _transitions match { case null => Nil ; case o => o }
	def transitions_=(t : List[Transition[_]]) { _transitions = t }
}

case class Transition[T](interpolation : Interpolation[T], setter : T => Unit, delay : UnitOfTime, duration : UnitOfTime) {
	var startTime : UnitOfTime = 0.seconds
}
