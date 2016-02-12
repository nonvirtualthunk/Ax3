package arx.axistential.modules.hunger

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 9/21/14
 * Time: 2:44 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.ai.PhysicalGoal
import arx.axistential.ai.TAIAgent
import arx.axistential.game.archetypes.effects._
import arx.axistential.game.entities.CreatureEntity
import arx.core.Moddable
import arx.core.query.ContinuousQueryListener
import arx.core.units.UnitOfTime

class HungerGameComponent extends AtIntervalGameEngineComponent with ContinuousQueryListener[CreatureEntity] {
	import HungerGameComponent._
	lazy val creatures = world.createEntityTypeQuery[CreatureEntity].withListener(this,fireOnExistingResults = true)


	override def interval = 10.seconds

	override def onInterval() = {

		for (creature <- creatures if creature.alive) {
			val currentCalories = creature.caloriesAvailable
			val activityLevel = creature match {
				case agent : TAIAgent => {
					agent.activeLeafGoal match {
						case Some(g) => g match {
							case pg : PhysicalGoal => pg.activityLevel
							case _ => ActivityLevel.None
						}
						case None => ActivityLevel.None
					}
				}
				case _ => Noto.warn(s"Creature that gets hungry but has no ai... $creature"); ActivityLevel.None
			}
			val calorieConsumption = creature.effectiveMetabolicRate(activityLevel)
			val nextCalories = currentCalories - calorieConsumption * interval.inSeconds

			val previousState = caloriesToHungerState(currentCalories,creature.baseMetabolicRate)
			val newState = caloriesToHungerState(nextCalories,creature.baseMetabolicRate)
			if (previousState != newState) {
				// remove all other hunger based statii
				val duplicateEffects = creature.statusEffects
					.filter(e => e.archetype.isInstanceOf[HungerArchetype] && e.causedBy == Some(HungerCause))
				duplicateEffects.foreach(_.removeFromCreature())

				for (newStatus <- newState) {
					// apply the hunger level status to the creature
					val statEffect = newStatus.effectArchetype.applyToCreature(creature)
					// with a built in end-condition that will remove it when they get enough calories
					// unless of course it is dead, feeding a dead person won't bring them back to life
					if (newStatus.effectArchetype != Dead) {
						statEffect.endCondition = (s : StatusEffect) => {
							val remaining = caloriesToTimeRemaining(creature.caloriesAvailable,creature.baseMetabolicRate)
							remaining >= newStatus.timeRemainingThreshold
						}
					}
					statEffect.causedBy = Some(HungerCause)
				}
			}

			creature.caloriesAvailable = nextCalories
		}
	}

	override def queryResultAdded(t: CreatureEntity): Unit = {
		// sentient creatures look for food differently
		if (t.species.sentient) {
			t.passiveGoals ::= new EatWhenHungry
		}
	}
	override def queryResultRemoved(t: CreatureEntity): Unit = {}
	
	case object HungerCause extends Cause.ConditionOfEntity("starvation")
}

object HungerGameComponent {
	protected case class HungerLevel (timeRemainingThreshold : UnitOfTime,effectArchetype : StatusEffectArchetype)

	val hungerLevels = List(
		HungerLevel(0.4.cycle,Hungry),
		HungerLevel(0.25.cycle,VeryHungry),
		HungerLevel(0.15.cycle,Starving),
		HungerLevel(0.0.cycle,Unconscious),
		HungerLevel(-0.15.cycle,Dead)
	).sortBy(_.timeRemainingThreshold.inSeconds)
	val maxLevel = HungerLevel(1.cycle,WellFed)

	def caloriesToHungerState (calories : Float,metabolicRate : Moddable[Float]) : Option[HungerLevel] = {
		val timeLeft = caloriesToTimeRemaining(calories,metabolicRate)

		hungerLevels.find(timeLeft < _.timeRemainingThreshold) match {
			case Some(level) => Some(level)
			case None if timeLeft >= maxLevel.timeRemainingThreshold => Some(maxLevel)
			case _ => None
		}
	}
	def caloriesToTimeRemaining(calories : Float,metabolicRate : Moddable[Float]) : UnitOfTime = {
		val ticksLeft = calories / metabolicRate.baseValue()
		(ticksLeft).seconds
	}
}