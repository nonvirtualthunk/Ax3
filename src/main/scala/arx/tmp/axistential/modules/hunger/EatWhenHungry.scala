package arx.axistential.modules.hunger

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 9/23/14
 * Time: 8:39 AM
 */

import arx.application.Noto
import arx.axistential.ai.AI.Reason.InsufficientItemsInInventory
import arx.axistential.ai.AI.Reason.Unfinished
import arx.axistential.ai._
import arx.axistential.game.archetypes.effects._
import arx.axistential.game.archetypes.species.AnimalSpecies
import arx.axistential.game.data.entity.animal.EatingData
import arx.axistential.game.data.entity.ClaimData
import arx.axistential.game.data.entity.FoodData
import arx.axistential.game.data.entity.InventoryData
import arx.axistential.game.logic.ai.goals.FetchRequirementsGoal
import arx.axistential.game.logic.ai.goals.animals.ForageGoal
import arx.axistential.game.logic.descriptors.FoodEntityDescriptor
import arx.axistential.game.logic.general.EntityLogic
import arx.core.units.UnitOfTime
import arx.requirements.NumberOfEntitiesRequirement

class EatWhenHungry extends PassiveGoal with TNonLocationConstrainedGoal {
	/* We're assuming here that sentient creatures and non-sentient creatures will treat
		lack of food in different ways. With the forethought and expectations that a human
		has, they will generally not spend every waking moment looking for their next meal
		unless quite hungry. Animals, on the other hand, will always be on the lookout for
		food unless they have some more pressing thing to be doing.
	 */
	val statusEffectToPriorityForSentients = Map(
		Hungry -> AI.Priority.Normal,
		VeryHungry -> AI.Priority.High,
		Starving -> AI.Priority.LifeOrDeath,
		Unconscious -> AI.Priority.AvoidingDeath
	).withDefaultValue(AI.Priority.Unimportant)

	val statusEffectToPriorityForAnimals = Map(
		Hungry -> AI.Priority.Normal,
		VeryHungry -> AI.Priority.High,
		Starving -> AI.Priority.LifeOrDeath,
		Unconscious -> AI.Priority.AvoidingDeath
	).withDefaultValue(AI.Priority.MidLow)

	val statusEffectToPriorityBySentience = Map(true -> statusEffectToPriorityForSentients,false -> statusEffectToPriorityForAnimals)

	def hungerStatusEffectForAgent(agent:TAIAgent) = agent.auxDataOpt[MetabolicData] match {
		case Some(md) =>
			HungerGameComponent.caloriesToHungerState(md.caloriesAvailable,md.baseMetabolicRate).map(_.effectArchetype)
		case None =>
			None
	}

	def sentient (agent:TAIAgent) = agent.archetype match { case s : AnimalSpecies => s.sentient ; case _ => false }

	override def priority(agent: TAIAgent): Int = {
		val map = statusEffectToPriorityBySentience(sentient(agent))
		hungerStatusEffectForAgent(agent) match {
			case Some(hs) => map(hs)
			case None =>
				// this will always give the default value, regardless of "Hungry" input
				map.default(Hungry)
		}
	}

	override def createPrerequisiteGoals(agent: TAIAgent): PrerequisiteGoalsResult = {
		agent.auxDataOpt[EatingData] match {
			case Some(ed) => {
				if (sentient(agent)) {
					// sentient creatures are assumed to just eat from stockpiles right now
					new FetchAndEatFoodGoal
				} else {
					// whereas animals are expected to forage
					new ForageGoal
				}
			}
			case None =>
				Noto.warn("EatWhenHungry for agent with no EatingData...")
				new FetchAndEatFoodGoal
		}
	}

	override def split(agent: TAIAgent): SplitResult = this

	override def act(agent: TAIAgent, dt: UnitOfTime): AIResult = {
		val finished =
			agent.aux[MetabolicData].caloriesAvailable >= agent.aux[MetabolicData].maxCaloriesStored * 0.95 ||
			(hungerStatusEffectForAgent(agent) match {
				case Some(sa) if sa == WellFed => true
				case _ => false
			})

		if (finished) { AIResult.Success }
		else { AIResult.Retry( Unfinished ) }
	}
}



/** Fetches food from any available inventory and eats that food */
class FetchAndEatFoodGoal extends EatFoodGoal {

	override def createPrerequisiteGoals(agent: TAIAgent): PrerequisiteGoalsResult = {
		FetchRequirementsGoal(foodRequirement,None,partialSuccessAllowed = false)
	}
}

/** Eat food from inventory */
class EatFoodGoal extends Goal {
	def foodRequirement = new NumberOfEntitiesRequirement(new FoodEntityDescriptor,1)

	override def createPrerequisiteGoals(agent: TAIAgent): PrerequisiteGoalsResult = Nil
	override def progressRequired(agent: TAIAgent): Float = 1.0f
	override def split(agent: TAIAgent): SplitResult = this
	override def fitness(agent: TAIAgent): Int = 0
	override def act(agent: TAIAgent, dt: UnitOfTime): AIResult = {
		val toEat = agent.aux[InventoryData].allHeldEntities
			.filter(e => e.aux[ClaimData].notClaimed)
			.filter(e => e.hasAuxData[FoodData])
			.filter(e => e.aux[FoodData].calories > 0)

		if (toEat.nonEmpty) {
			for (edible <- toEat) {
				EntityLogic.destroyEntity(edible)
				agent.aux[MetabolicData].caloriesAvailable += edible.aux[FoodData].calories
				Noto.finest(MetabolicLogging,s"$agent eating food item $edible, gaining ${edible.aux[FoodData].calories} calories, new total ${agent.aux[MetabolicData].caloriesAvailable}")
			}
			AIResult.Success
		} else {
			AIResult.Fail(InsufficientItemsInInventory(foodRequirement,agent))
		}
	}
}