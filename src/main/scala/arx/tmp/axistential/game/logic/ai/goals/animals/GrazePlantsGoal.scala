package arx.axistential.game.logic.ai.goals.animals

import arx.application.Noto
import arx.axistential.ai._
import arx.axistential.game.data.entity.FoodData
import arx.axistential.game.data.entity.plants.PlantData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.ai.goals.MoveGait
import arx.axistential.game.logic.ai.goals.MoveToRangeOfEffectGoal
import arx.axistential.game.logic.general.EntityLogic
import arx.axistential.modules.hunger.ActivityLevel
import arx.axistential.modules.hunger.MetabolicData
import arx.core.datastructures.OneOrMore
import arx.core.units.UnitOfTime
import arx.tmp.game.logic.descriptors.TDescriptor

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/30/15
 * Time: 5:43 PM
 */

class GrazePlantsGoal(plants : OneOrMore[TPhysicalEntity], allowableParts : Set[TDescriptor]) extends PhysicalGoal {
	override def activityLevel: ActivityLevel = ActivityLevel.Light

	override def createPrerequisiteGoals(agent: TAIAgent): PrerequisiteGoalsResult =
		MoveToRangeOfEffectGoal(agent,plants.head,MoveGait.Saunter) :: Nil
	override def progressRequired(agent: TAIAgent): Float = 4.0f
	override def split(agent: TAIAgent): SplitResult = plants.size match {
		case 1 => this
		case _ => new GrazePlantsGoal(plants.head,allowableParts) -> new GrazePlantsGoal(plants.tail,allowableParts)
	}
	override def fitness(agent: TAIAgent): Int = AI.Fitness.Normal
	override def act(agent: TAIAgent, dt: UnitOfTime): AIResult = {
		progress += dt.inSeconds
		if (progress >= progressRequired(agent)) {
			val plant = plants.head
			val PD = plant.aux[PlantData]
			val (edibleParts,remainingParts) = PD.parts
				.partition(p => allowableParts.exists(_.matches(p.entity)))
			var tc = 0.0f
			for (part <- edibleParts) {
				val calories = part.entity.auxDataOpt[FoodData] match {
					case Some(fd) => fd.calories
					case None => Noto.warn("Something is eating a non-food entity, we want to handle that eventually, but right now it doesn't have good logic");100.0f
				}

				tc += calories
				EntityLogic.destroyEntity(part.entity)
				agent.aux[MetabolicData].caloriesAvailable += calories
			}
			Noto.info(s"Ate $tc calories, new remaining available : ${agent.aux[MetabolicData].caloriesAvailable}")
			PD.parts = remainingParts
			Success
		} else {
			Continue
		}
	}
}
