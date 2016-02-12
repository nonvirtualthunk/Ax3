package arx.axistential.game.archetypes.species

import arx.application.Noto
import arx.axistential.game.data.entity.AgeCategory
import arx.axistential.game.data.entity.SpeciesKind
import arx.core.representation.ConfigValue
import arx.resource.ResourceManager

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/18/15
 * Time: 8:08 PM
 */

trait TSpeciesKindCollector[T <: SpeciesKind] {
	def parseKindAgeCategory(conf : ConfigValue,kind : T,ac : AgeCategory)
	def confLocation : String
	def kindFromName(name:String) : T

	private val sml = ResourceManager.sml(confLocation)
	for ((namespace,topLevelConfig) <- sml.fields; (name,kindSml) <- topLevelConfig.PlantKinds.fields) {
		val k = kindFromName(name)

		var prev : Option[AgeCategory] = None
		for (acSml <- kindSml.ageCategories.arr) {
			val ac = AgeCategory(acSml.name.str,acSml.adjective.str,None)

			parseKindAgeCategory(acSml,k,ac)

			k.ageCategories :+= ac
			for (pac <- prev) { pac.followedBy = Some(ac) }
			prev = Some(ac)
		}

		k.matureAgeCategory = k.ageCategories.find(ac => ac.name == kindSml.matureAgeCategory.strOrElse("")) match {
			case Some(ac) => ac
			case None =>
				Noto.warn(s"No matureAgeCategory specified for $k, falling back on end of age categories")
				k.ageCategories.last
		}
		k.matureAgeCategory.maturityAgeCategory = true
		k.defaultAgeAfterMaturityMultiplier = 1.0f / (1.0f - kindSml.defaultLifeAfterMaturityProportion.floatOrElse(0.5f))
	}
}
