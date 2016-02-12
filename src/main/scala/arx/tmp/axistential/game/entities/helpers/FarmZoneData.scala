package arx.axistential.game.entities.helpers

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/3/15
 * Time: 8:18 AM
 */

import arx.axistential.game.archetypes.species.PlantSpecies
import arx.axistential.game.data.entity.AgeCategory
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.core.representation.InformationLevel.InformationLevel
import arx.core.vec.coordinates.TMajorCoord
import arx.tmp.game.logic.entities.data.TGameEntityAuxData

class FarmZoneData extends TGameEntityAuxData {
	/** A record of those plants that have been sown in this field and are still present */
	protected var _sownPlants : List[TPhysicalEntity] = Nil
	def sownPlants = { _sownPlants = _sownPlants.filter(_.inWorld); _sownPlants }
	def sownPlants_= (s : List[TPhysicalEntity]) { _sownPlants = s }
	/** Will attempt to plant the given species throughout this zone, if specified */
	var speciesToPlant : Option[PlantSpecies] = None
	/** Uses the plant kind's age of maturity unless otherwise specified */
	var harvestAtAgeCategory : Option[AgeCategory] = None
	/** Target locations to sow plants */
	var plantAtLocations : List[TMajorCoord] = Nil


	override def informationPairs(level: InformationLevel): Map[String, Any] = Map()
}
