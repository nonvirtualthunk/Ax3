package arx.axistential.game.data.entity.plants

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/11/15
 * Time: 7:59 AM
 */

import arx.Prelude._
import arx.axistential.game.archetypes.species.PlantSpecies
import arx.core.Moddable
import arx.core.representation.ConfigValue
import arx.core.units.UnitOfDistance
import arx.core.units.UnitOfMeasure
import arx.tmp.game.logic.entities.data.TConfigurableGameEntityAuxData
import arx.tmp.game.logic.entities.data.TInheritableAuxData

class PlantSeedData extends TConfigurableGameEntityAuxData with TInheritableAuxData {
	var forSpecies : Moddable[PlantSpecies] = PlantSpecies.Sentinel
	var travelDistance : UnitOfDistance = 1.meter


	def createFromSML(sml: ConfigValue): Option[PlantSeedData] = {
		if ( sml.seedOf.nonEmpty ) {
			val d = new PlantSeedData
			d.forSpecies = () => PlantSpecies.archetypeWithName( sml.seed.str )
			d.travelDistance = UnitOfMeasure.parseUnitOfDistance(sml.travelDistance.str)
			Some(d)
		} else { None }
	}
}
