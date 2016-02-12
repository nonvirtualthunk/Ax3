package arx.axistential.game.logic.ai.groupgoals

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/9/15
 * Time: 8:05 AM
 */

import arx.Prelude._
import arx.axistential.ai.AIGroupGoal
import arx.axistential.ai.Goal
import arx.axistential.ai.TAIGroup
import arx.axistential.game.data.entity.plants.PlantData
import arx.axistential.game.entities.helpers.FarmZoneData
import arx.axistential.game.entities.helpers.Zone
import arx.axistential.game.logic.ai.goals.HarvestEntitiesGoal
import arx.core.units.UnitOfTime
import arx.tmp.game.logic.descriptors.AnyEntityDescriptor

class HarvestFarmGoal(farm : Zone) extends AIGroupGoal {
	val FD = farm.aux[FarmZoneData]
	
	/** Create a new goal to be executed by the colony, if necessary, or None otherwise */
	override def createSubGoal(forGroup: TAIGroup): Option[Goal] = {
		FD.harvestAtAgeCategory match {
			case Some(ac) => {
				val plantsOfAge = FD.sownPlants.filter(p => p.aux[PlantData].ageCategory == ac)
				plantsOfAge match {
					case Nil => None
					case _ => {
						Some(new HarvestEntitiesGoal(plantsOfAge,AnyEntityDescriptor,true))
					}
				}
			}
			case None => {
				// If we don't have any age category to harvest at, we assume this operates like an orchard,
				// we collect harvestable parts when they are ready, but we don't harvest the whole plant

				// collect a mapping of the harvestable parts of each plant that are non-structural
				val plantsWithHarvestableParts = FD.sownPlants
					.filter(p => p.aux[PlantData].parts.exists(! _.structural))
				plantsWithHarvestableParts match {
					case Nil => None
					case l => Some(new HarvestEntitiesGoal (l, AnyEntityDescriptor, false))
				}
			}
		}
	}

	/** How often this should be checked for new work that needs to be done */
	override def checkInterval: UnitOfTime = 10.seconds

	override def toString: String = s"HarvestFarmGoal($farm)"
}
