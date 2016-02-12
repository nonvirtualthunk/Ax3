package arx.axistential.game.logic.ai.goals

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/3/15
 * Time: 9:15 AM
 */

import arx.Prelude._
import arx.axistential.ai.AI.Reason.InsufficientItemsInInventory
import arx.axistential.ai._
import arx.axistential.game.archetypes.species.PlantSpecies
import arx.axistential.game.data.entity.ClaimData
import arx.axistential.game.data.entity.InventoryData
import arx.axistential.game.data.entity.NoInventory
import arx.axistential.game.entities.helpers.FarmZoneData
import arx.axistential.game.entities.helpers.Zone
import arx.axistential.game.logic.descriptors.SeedForDescriptor
import arx.axistential.game.logic.general.PlantLogic
import arx.axistential.modules.hunger.ActivityLevel
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.TMajorCoord
import arx.tmp.game.logic.entities.core.TSanityCheckable.Assert
import arx.requirements.NumberOfEntitiesRequirement

class SowSeedsGoal(plantSpecies : PlantSpecies,atLocations : List[TMajorCoord],inZone : Zone) extends PhysicalGoal {
	override def activityLevel: ActivityLevel = ActivityLevel.Light

	protected var fetchInitiated = false
	protected def withFetchInitiated = {fetchInitiated = true; this}
	
	protected def seedRequirement = {
		val seedDesc = new SeedForDescriptor(plantSpecies)
		new NumberOfEntitiesRequirement(seedDesc,atLocations.size)
	}

	override def createPrerequisiteGoals(agent: TAIAgent): PrerequisiteGoalsResult = {
		MoveToRangeOfEffectGoal(agent,atLocations.head.toVoxelCoord) :: Nil
	}

	override def progressRequired(agent: TAIAgent): Float = 5

	// TODO: This is likely to be a common pattern, fetch items to location then do work, abstract it out when we have
	// a solid idea how we want it to work
	override def split(agent: TAIAgent): SplitResult = {
		if (!fetchInitiated) {
			
			val copy = new SowSeedsGoal(plantSpecies,atLocations,inZone)

			val fetchGoal = FetchRequirementsGoal(seedRequirement,Some(inZone),partialSuccessAllowed = false)
			fetchGoal andThen copy.withFetchInitiated
		} else {
			atLocations match {
				case single :: Nil => this
				case allLocs =>
					val sortedLocs = allLocs.sortBy(l => agent.position.scalarDistanceTo(l))
					new SowSeedsGoal(plantSpecies,sortedLocs.take(1),inZone).withFetchInitiated ->
						new SowSeedsGoal(plantSpecies,sortedLocs.drop(1),inZone).withFetchInitiated
			}
		}
	}


	override def plan(agent: TAIAgent): AIResult = {
		val inventory = inZone.auxDataOrElse[InventoryData](NoInventory)
		val (entities,amount) = inventory.entitiesToFulfillRequirement(seedRequirement,None,agent,this)
		posit(atLocations.size == 1,"When at planning stage, SowSeedGoal should always have 1 location")
		if (amount >= atLocations.size) {
			AIResult.Success
		} else {
			AIResult.Abort(new InsufficientItemsInInventory(seedRequirement,inZone))
		}
	}

	override def fitness(agent: TAIAgent): Int = AI.Fitness.Normal

	override def act(agent: TAIAgent, dt: UnitOfTime): AIResult = {
		progress += dt.inSeconds
		if (progress >= progressRequired(agent)) {
			val inventory = inZone.aux[InventoryData]
			inventory.allHeldEntities.find(e => e.aux[ClaimData].claimedFor == Some(this)) match {
				case Some(seed) => {
					val newPlant = PlantLogic.createPlant(plantSpecies,agent.world,atLocations.head,Some(seed))

					for (fd <- inZone.auxDataOpt[FarmZoneData]) {
						fd.sownPlants ::= newPlant
					}

					Success
				}
				case None => AIResult.Fail(new InsufficientItemsInInventory(seedRequirement,inZone))
			}
		} else {
			Continue
		}
	}

	override def sanityChecks: List[Assert] = Assert(atLocations.nonEmpty,"SowSeedsGoal with empty location list") :: super.sanityChecks
}
