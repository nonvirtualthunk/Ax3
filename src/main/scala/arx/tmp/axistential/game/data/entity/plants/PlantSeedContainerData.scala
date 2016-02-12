package arx.axistential.game.data.entity.plants

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/11/15
 * Time: 8:00 AM
 */

import arx.axistential.game.archetypes.item.ConfiguredItemArchetype
import arx.axistential.game.archetypes.item.ItemArchetype
import arx.axistential.game.archetypes.species.SeedPart
import arx.core.representation.ConfigValue
import arx.tmp.game.logic.entities.data.TConfigurableGameEntityAuxData
import arx.tmp.game.logic.entities.data.TInheritableAuxData

class PlantSeedContainerData extends TConfigurableGameEntityAuxData with TInheritableAuxData {
	var seedParts : List[SeedPart] = Nil

	def createFromSML(sml: ConfigValue): Option[PlantSeedContainerData] = {
		val seedPartSMLarr = extractSingularOrPlural(sml,"seedPart","seedParts")
		if ( seedPartSMLarr.nonEmpty ) {
			val d = new PlantSeedContainerData

			for ( partSML <- seedPartSMLarr ) {
				val arch = ItemArchetype.registerArchetype( partSML.name.str, new ConfiguredItemArchetype(partSML) )
				val number = partSML.number.intOrElse(1)
				d.seedParts ::= SeedPart(arch,number)
			}

			Some(d)
		} else { None }
	}
}
