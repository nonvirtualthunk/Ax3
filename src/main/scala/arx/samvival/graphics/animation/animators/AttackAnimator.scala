package arx.samvival.graphics.animation.animators

import arx.engine.event.GameEvent
import arx.samvival.game.events.GameEvents
import arx.samvival.graphics.animation.{Animation, TextAnimationElement}
import arx.Prelude._
import arx.core.datastructures.{AutoTraversable, OneOrMore}
import arx.core.math.Interpolation
import arx.core.vec.{ReadVec4f, Vec3f, Vec4f}
import arx.core.vec.coordinates.CartVec3
import arx.engine.data.Reduceable
import arx.engine.lworld.EventState.{Ended, Started}
import arx.engine.lworld.{EventState, LWorldView}
import arx.graphics.helpers.{HSBA, RichText, TextSection}
import arx.samvival.game.entities
import arx.samvival.game.entities.Fields.{CharacterInfo, Physical}
import arx.samvival.game.entities.StrikeOutcome.{Armored, Blocked, Dodged, Miss}
import arx.samvival.game.entities.{Physical, StrikeResult}
import arx.samvival.graphics.animation.PropertyAnimationElement.AnimateableEntity


class AttackAnimator extends EventAnimator {

	import Interpolation._

	override def createAnimationFor(implicit view: LWorldView): PartialFunction[(GameEvent, EventState), AutoTraversable[Animation]] = {
		case (GameEvents.Strike(attacker, defender, strike, resultOpt), startEnd) =>
			val startPos = attacker[Physical].position.asCartesian
			val defenderPos = defender[Physical].position.asCartesian
			val delta = (defenderPos - startPos).normalizeSafe
			val targetPos = startPos + CartVec3(delta) * 0.5f

			startEnd match {
				case Started =>
//					Animation(0.41.seconds, 0.4.seconds)
//						.withElement(attacker.animate(Physical.exactPositionOverride, between(startPos, targetPos).sin01.map(Option(_)))) :: Nil
					Animation(0.8.seconds, 0.4.seconds)
						.withElement(attacker.animate(Physical.exactPositionOverride, between(startPos, targetPos).sin010.map(Option(_))))
				case Ended =>
					resultOpt match {
						case Some(StrikeResult(outcomes, _)) => {
							if (outcomes.contains(Dodged)) {
								val ortho = CartVec3(Vec3f.UnitZ.cross(delta))
								Animation(0.5.seconds, 0.5.seconds)
   								.withElement(defender.animate(Physical.exactPositionOverride, between(defenderPos, defenderPos + ortho * 0.3f).sin010.map(Option(_))))
							} else {
								val text = if (outcomes.contains(Blocked)) {
									"Blocked"
								} else if (outcomes.contains(Armored)) {
									"Armored"
								} else if (outcomes.contains(Miss)) {
									"Miss"
								} else {
									""
								}

								if (text.nonEmpty) {
									Animation(1.5.seconds, 0.2.seconds)
										.withElement(0.0f,1.0f, TextAnimationElement(between(defenderPos.plusY(0.25f), defenderPos.plusY(0.75f)), 0.75f, between(HSBA.White, HSBA.Clear), RichText(text)))
								} else { Nil }
							}
						}
					}
//					Animation(0.4.seconds, 0.4.seconds)
//						.withElement(attacker.animate(Physical.exactPositionOverride, between(startPos, targetPos).sin10.map(Option(_)))) :: Nil
//					Nil
				case _ => Nil
			}
	}
}


class DamageAnimator extends EventAnimator {
	import Interpolation._

	override def createAnimationFor(implicit view: LWorldView): PartialFunction[(GameEvent, EventState), AutoTraversable[Animation]] = {
		case (GameEvents.DamageTaken(defender, damage), Ended) =>
			val curHealth = defender[entities.CharacterInfo].health

			val defenderPos = defender[Physical].position.asCartesian
			val damageText = RichText(TextSection(s"${damage.amount}", Vec4f(0.8f, 0.2f, 0.2f, 1.0f)) :: Nil)
			Animation(1.5.seconds, 0.75.seconds)
				.withElement(0.0f, 0.3f, defender.animate(Physical.colorMultiplier, Interpolation.between(HSBA.White, HSBA.fromRGBA(1.0f, 0.0f, 0.0f, 1.0f)).sin010))
				.withElement(0.15f, 1.0f, TextAnimationElement(between(defenderPos + CartVec3(0.0f, 0.25f, 0.0f), defenderPos + CartVec3(0.0f, 0.75f, 0.0f)), 0.75f, between(HSBA.White, HSBA.Clear), damageText))
				.withElement(0.0f, 0.52f, defender.animate(CharacterInfo.health, betweenI(curHealth, curHealth.reduceBy(damage.amount, limitToZero = true))))
	}
}