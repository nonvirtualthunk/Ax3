package arx.axistential.game.logic.ai.goals

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/30/14
 * Time: 2:19 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.ai.AI.Reason.InsufficientItemsInInventory
import arx.axistential.ai.AI.Reason.UnexpectedState
import arx.axistential.ai._
import arx.axistential.game.data.entity.InventoryData
import arx.axistential.game.data.entity.LivingThingData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.general.EntityLogic
import arx.axistential.modules.hunger.ActivityLevel
import arx.core.datastructures.OneOrMore
import arx.core.units.UnitOfTime
import arx.tmp.game.logic.descriptors.TDescriptor
import arx.requirements.NoRequirement
import arx.requirements.TRequirement

case class HarvestEntitiesGoal (entities : OneOrMore[TPhysicalEntity], harvestDescriptor : TDescriptor, fullHarvest : Boolean) extends PhysicalGoal {
	override def activityLevel: ActivityLevel = if (fullHarvest) { ActivityLevel.Moderate } else { ActivityLevel.Light }

	lazy val toolRequirement = requirementForEntity(entities.head)

	override def createPrerequisiteGoals(agent: TAIAgent): PrerequisiteGoalsResult = {
		// fetch the tool, but don't take it anywhere (the agent will use it)
		val toolGoal = FetchRequirementsGoal(toolRequirement,None,partialSuccessAllowed = false)
		// and move to the entity to be harvested
		val moveGoal = MoveToRangeOfEffectGoal(agent,entities.head)

		toolGoal :: moveGoal :: Nil
	}

	override def progressRequired(agent: TAIAgent): Float = {
		val ent = entities.head
		val volume = ent.boundingDimensions.volume.inCubicMeters
		(volume * 10.0f).max(5)
	}

	override def split(agent: TAIAgent): SplitResult = {
		if (entities.size > 1) {
			val sortedEntities = entities.toList.sortBy(_.position.scalarDistanceTo(agent.position))
			HarvestEntitiesGoal(sortedEntities.head,harvestDescriptor,fullHarvest) ->
				HarvestEntitiesGoal(sortedEntities.tail,harvestDescriptor,fullHarvest)
		} else {
			this
		}
	}

	override def fitness(agent: TAIAgent): Int = if (hasNecessaryTools(agent)) {
		AI.Fitness.SomewhatFit
	} else {
		0
	}

	def hasNecessaryTools (agent : TAIAgent) = {
		agent.aux[InventoryData].couldFulfillRequirement(toolRequirement)
	}

	override def act(agent: TAIAgent, dt: UnitOfTime): AIResult = {
		if (!hasNecessaryTools(agent)) {
			Abort(InsufficientItemsInInventory(toolRequirement,agent))
		} else if (!entities.head.inWorld) {
			Fail(UnexpectedState("entity to harvest no longer exists"))
		} else {
			progress += dt.inSeconds
			if (progress > progressRequired(agent)) {
				val ent = entities.head
				ent.auxDataWithTrait[LivingThingData] match {
					case Some(ld) =>
						val partsToHarvest = partsToHarvestFrom(ent)

						val ID = agent.aux[InventoryData]
						for (part <- partsToHarvest; ent = part.entity) {
							// attempt to pick up the relevant part, but if there isn't space, drop it on the ground
							val couldHold = ID.holdEntityIfPossible(ent)
							if (!couldHold) {
								ent.position = agent.position
								agent.world.addEntity(ent)
							}
							ld.parts = ld.parts without part
						}

						if (fullHarvest) {
							EntityLogic.destroyEntity(ent)
						}

						Success
					case None =>
						Fail(AI.Reason.InvalidTarget(ent,"Target lacked living thing data, could not harvest"))
				}
			} else {
				Continue
			}
		}
	}
	
	def partsToHarvestFrom (ent : TPhysicalEntity) = {
		ent.auxDataWithTrait[LivingThingData] match {
			case Some(ld) => ld.parts.filter(p => (!p.structural || fullHarvest) && harvestDescriptor.matches(p.entity))
			case None => Noto.warn("Attempting to harvest from something without appropriate LivingThingData"); Nil
		}
	}

	def requirementForEntity (ent : TPhysicalEntity) : TRequirement = {
		val parts = partsToHarvestFrom(ent)
		val reqs = parts.map(_.harvestableBy).filterNot(_.isSentinel)
		reqs match {
			case Nil => NoRequirement
			case _ => reqs.reduceLeft(_.and(_))
		}
	}
}
