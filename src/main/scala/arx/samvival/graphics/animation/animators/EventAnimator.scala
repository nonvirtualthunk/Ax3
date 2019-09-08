package arx.samvival.graphics.animation.animators

import arx.core.vec.coordinates.CartVec3
import arx.engine.event.GameEvent
import arx.samvival.game.entities.Fields.Physical
import arx.samvival.game.events.GameEvents
import arx.samvival.graphics.animation.{Animation, AnimationData}
import arx.Prelude._
import arx.core.datastructures.{AutoTraversable, OneOrMore}
import arx.engine.lworld.{EventState, LWorldView}
import arx.samvival.graphics.animation.PropertyAnimationElement.AnimateableEntity

trait EventAnimator {
	def createAnimationFor(implicit view : LWorldView) : PartialFunction[(GameEvent, EventState), AutoTraversable[Animation]]
}

class MoveEventAnimator extends EventAnimator {
	override def createAnimationFor(implicit view : LWorldView) = {
		case (GameEvents.EntityMoved(entity, from, to), EventState.Ended) =>
			val oldPos = from.asCartesian
			val newPos = to.asCartesian

			val delta = newPos - oldPos

			Animation(0.5.seconds, 0.5.seconds)
				.withElement(entity.animate(Physical.exactPositionOverride, (pcnt: Float) => Some(oldPos + delta * pcnt): Option[CartVec3])) :: Nil
	}
}