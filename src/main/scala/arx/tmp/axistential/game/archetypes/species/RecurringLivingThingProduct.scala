package arx.axistential.game.archetypes.species

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/16/15
 * Time: 9:02 AM
 */

import arx.axistential.game.archetypes.traits.TPhysicalEntityArchetype
import arx.axistential.game.data.entity.AgeCategory
import arx.axistential.game.data.world.Season
import arx.core.Moddable
import arx.core.units.UnitOfTime
import arx.requirements.TRequirement

object RecurringLivingThingProduct {
	 def apply (arch : TPhysicalEntityArchetype ,
					seasons : Set[Season],
					atAgeCategory : AgeCategory,
					numberPerProduction : Moddable[Float],
					productionInterval : UnitOfTime ,
					structural : Boolean,
					harvestableBy : TRequirement) = new RecurringLivingThingProduct(arch,seasons,atAgeCategory,numberPerProduction,productionInterval,structural,harvestableBy)
 }

class RecurringLivingThingProduct(
	 arch : TPhysicalEntityArchetype ,
	 val seasons : Set[Season],
	 atAgeCategory : AgeCategory,
	 val numberPerProduction : Moddable[Float],
	 val productionInterval : UnitOfTime ,
	 struct : Boolean,
	 harvestBy : TRequirement,
	 val maximumCount : Option[Int] = None
 ) extends LivingThingProduct(arch,struct,atAgeCategory,numberPerProduction,harvestBy) {}