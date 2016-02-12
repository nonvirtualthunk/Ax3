package arx.axistential.game.archetypes.species

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/16/15
 * Time: 9:18 AM
 */

import arx.axistential.game.data.entity.AgeCategory
import arx.axistential.game.data.entity.SpeciesKind
import arx.core.representation.ConfigValue
import arx.core.traits.ArxEnum
import arx.core.traits.ArxEnumObject
import arx.resource.ResourceManager


class AnimalKind(name : String) extends ArxEnum(name) with SpeciesKind {
	private val sml = ResourceManager.sml("axis/entities/animals/AnimalKinds.sml")
	for ((namespace,topLevelConfig) <- sml.fields; (name,kindConf) <- topLevelConfig.AnimalKinds.fields) {
		
	}
}

object AnimalKind extends ArxEnumObject[AnimalKind] with TSpeciesKindCollector[AnimalKind] {
	override def parseKindAgeCategory(conf: ConfigValue, kind: AnimalKind, ac: AgeCategory): Unit = {
		// do nothing further, we're good, this is where we would do harvestToolByAgeCategory
	}
	override def confLocation: String = "axis/entities/animals/AnimalKinds.sml"
	override def kindFromName(name: String): AnimalKind = AnimalKind(name)

	val DefaultAnimal = AnimalKind("Animal")
}



