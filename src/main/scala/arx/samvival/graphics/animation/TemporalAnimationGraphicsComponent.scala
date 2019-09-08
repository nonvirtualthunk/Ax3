package arx.samvival.graphics.animation

import arx.Prelude._
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.CartVec
import arx.engine.advanced.lenginecomponents.LGraphicsComponent
import arx.engine.advanced.lenginepieces.LGraphicsEngine
import arx.samvival.game.entities.Fields.Physical
import arx.samvival.game.entities.Physical
import arx.samvival.game.events.GameEvents
import arx.samvival.game.events.GameEvents.EntityMoved
import arx.samvival.graphics.animation.PropertyAnimationElement.AnimateableEntity

class TemporalAnimationGraphicsComponent(engine : LGraphicsEngine) extends LGraphicsComponent(engine) {
	override def draw(): Unit = {}

	override protected def updateSelf(dt: UnitOfTime): Unit = {
//		val animationData = graphics[AnimationData]
//		// if the animation queue still has stuff in it, just do that, nothing further
//		if (animationData.animationQueue.isEmpty) {
//			// if we're not blocked on animations, and we're not caught up to the current state of truth
//			if (engine.worldCore.currentTime > world.currentTime) {
//				// if we haven't animated up to the next
//				if (animationData.lastAnimated < world.currentTime + 1) {
//					engine.worldCore.eventAt(animationData.lastAnimated + 1) match {
//						case Some(event) =>
//							event.event pmatch {
//								case EntityMoved(entity, from, to) =>
//									val oldPos = from.asCartesian
//									val newPos = to.asCartesian
//
//									val delta = newPos - oldPos
//
//									graphics[AnimationData] registerAnimation Animation(1.seconds, 1.seconds)
//										.withElement(0.0f, 1.0f, entity.animate(Physical.exactPositionOverride, (pcnt: Float) => Some(CartVec(oldPos + delta * pcnt)): Option[CartVec]))
//									animationData.lastAnimated += 1
//							}
//						case None =>
//
//					}
//				}
//				if (engine.worldCore.currentTime > world.currentTime) {
//					// advance the world by one
//					engine.worldCore.updateViewToTime(world, world.currentTime + 1)
//				}
//			}
//		}
	}
}
