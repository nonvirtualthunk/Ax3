package arx.samvival.graphics.animation.animators

import arx.core.datastructures.AutoTraversable
import arx.engine.event.GameEvent
import arx.engine.lworld.{EventState, LWorldView}
import arx.samvival.game.events.GameEvents
import arx.samvival.graphics.animation.Animation
import arx.Prelude._
import arx.core.math.Interpolation
import arx.graphics.helpers.HSBA
import arx.samvival.game.entities.Fields.Physical
import arx.samvival.graphics.animation.PropertyAnimationElement.AnimateableEntity

class DeathAnimator extends EventAnimator {
	override def createAnimationFor(implicit view: LWorldView): PartialFunction[(GameEvent, EventState), AutoTraversable[Animation]] = {
		case (GameEvents.Died(character), EventState.Started) =>
			val baseColor = character(Physical).colorMultiplier
			Animation(0.8.seconds, 0.75.seconds).withElement(character.animate(Physical.colorMultiplier, Interpolation.between(baseColor, baseColor.withA(0.0f))))
	}
}
