package arx.axistential.game.logic.ai.goals.animals

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 4/11/15
 * Time: 10:39 AM
 */

import arx.Prelude._
import arx.axistential.ai.AI.Reason.InvalidAgent
import arx.axistential.ai._
import arx.axistential.ai.traits.TSingularGoal
import arx.axistential.game.data.entity.animal.AnimalData
import arx.axistential.game.data.entity.AIData
import arx.axistential.game.data.entity.FoodData
import arx.axistential.game.entities.CreatureEntity
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.ai.goals.HarvestEntitiesGoal
import arx.axistential.game.logic.ai.goals.MoveGait
import arx.axistential.game.logic.descriptors.FoodEntityDescriptor
import arx.axistential.game.logic.general._
import arx.axistential.modules.hunger.ActivityLevel
import arx.axistential.modules.hunger.EatFoodGoal
import arx.core.units.UnitOfDistance
import arx.core.units.UnitOfTime

class HuntAnimalGoal(targetAnimal : CreatureEntity) extends PhysicalGoal {
	override def activityLevel: ActivityLevel = ActivityLevel.Heavy
	override def progressRequired(agent: TAIAgent): Float = 0.0f

	/** If this goal can be split into more than one part, create two new goals, the first representing
	  * the split off portion, the second representing the remainder
	  * @param agent the agent who is to perform the split off section
	  */
	override def split(agent: TAIAgent): SplitResult = this
	override def createPrerequisiteGoals(agent: TAIAgent): PrerequisiteGoalsResult = {
		/* Here we need to decide on what our strategy for catching our target is going to be. The worst strategy we
			have is just to run straight after it. If we aren't stealthy, or they've already spotted us, that may be the
			best we can do though. Generally though, we should be trying to sneak up to the creature, then at some point
			switch to closing with it as fast as we can. Ideally that should be before it has sighted us so we can get
			accelerated up to max speed before they do.
		 */
		new KillEnemyGoal(targetAnimal, 100.meters, targetAnimal.maximumObservationRange * 1.1f,false)
	}

	override def fitness(agent: TAIAgent): Int = AI.Fitness.Normal

	override def act(agent: TAIAgent, dt: UnitOfTime): AIResult = {
		Success
	}
}


class KillEnemyGoal (target : CreatureEntity, totalDistanceAllowed : UnitOfDistance,maxRangeToTarget : UnitOfDistance, sneak : Boolean) extends PhysicalGoal with TSingularGoal {
	override def activityLevel: ActivityLevel = ActivityLevel.Heavy
	override def progressRequired(agent: TAIAgent): Float = 0.1f

	override def createPrerequisiteGoals(agent: TAIAgent): PrerequisiteGoalsResult = {
		if (PhysicalEntityLogic.minimumDistanceBetweenEntities(agent,target) < 2.voxels) {
			Nil
		} else {
			val targetAID = target.aux[AIData]
			agent.expectAs[CreatureEntity] match {
				case Some(hunter) =>
					if (targetAID.detectedCreatures.contains(hunter)) {
						new ChaseGoal(target, 100.meters, target.maximumObservationRange * 1.1f,false, MoveGait.Sprint)
						// Otherwise, attempt to sneak up
					} else {
						new ChaseGoal(target, 100.meters, target.maximumObservationRange * 1.1f,true, MoveGait.Sprint)
					}
				case None => InvalidAgent(agent,"Should have been creature")
			}
		}
	}
	override def fitness(agent: TAIAgent): Int = AI.Fitness.Normal
	override def act(agent: TAIAgent, dt: UnitOfTime): AIResult = {
		// As long as we're within reach at the start, it's ok
		if (progress > 0.0f || PhysicalEntityLogic.minimumDistanceBetweenEntities(agent,target) < 2.voxels) {
			progress += dt.inSeconds
			if (progress > progressRequired(agent)) {
				EntityLogic.destroyEntity(target)
				Success
			} else { Continue }
		} else { Retry(AI.Reason.AgentInInvalidPosition(agent.asInstanceOf[TAIAgent with TPhysicalEntity],target.position,agent.position)) }
	}
}


class EatAnimalGoal(corpse : CorpseEntity) extends PhysicalGoal with TSingularGoal {
	override def activityLevel: ActivityLevel = ActivityLevel.Light
	override def progressRequired(agent: TAIAgent): Float = 0.0f
	override def plan(agent: TAIAgent): AIResult = super.plan (agent)
	override def onReset(): Unit = {}

	override def createPrerequisiteGoals(agent: TAIAgent): PrerequisiteGoalsResult = {
		val edibleParts = corpse.aux[AnimalData].parts.filter(_.entity.hasAuxData[FoodData])

		if (edibleParts.nonEmpty) {
			// we don't need it to be a full harvest because when something becomes a corpse we set all its parts
			// as non-structural
			HarvestEntitiesGoal(corpse,new FoodEntityDescriptor,fullHarvest = false) ::
			new EatFoodGoal :: Nil
		} else {
			AI.Reason.InvalidTarget(corpse,"Target did not have any edible parts to harvest")
		}
	}

	override def fitness(agent: TAIAgent): Int = 0
	override def act(agent: TAIAgent, dt: UnitOfTime): AIResult = {
		Success
	}
}