package arx.tmp.game.logic.universe.generation

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/14/13
 * Time: 4:25 PM
 * Created by nonvirtualthunk
 */

import arx.core.language.LanguageData.Language
import arx.tmp.game.logic.entities.data.Sex
import arx.tmp.game.logic.universe.data.LanguageData
import arx.tmp.game.logic.universe.TUniverseDataGenerator
import arx.tmp.game.logic.universe.Universe
import arx.tmp.game.logic.universe.UniverseGenerator
import arx.resource.ResourceManager

class UniverseLanguageGenerator extends TUniverseDataGenerator {
	def generateData(universe: Universe) {
		val languageData = universe.auxData[LanguageData]

		val baseLanguageIdentifiers = List("elvish" -> false,"irish" -> false)
		val baseLanguages = baseLanguageIdentifiers.map {
			case(languageBaseName,binary) => ResourceManager.languageBasis("axis/language/" + languageBaseName + "/" + languageBaseName,binary)
		}

		val synthesizedLanguage = Language.synthesize( Map(baseLanguages.head -> 0.6f , baseLanguages.last -> 0.4f) )
		languageData.languages ::= synthesizedLanguage

		println("NAMES:")
		for ( i <- 0 until 20 ) {
			val name = synthesizedLanguage.generatePersonName(Sex.Neuter,Map())
			println(name.personalName)
		}
		println("DONE")
	}
}

object UniverseLanguageGenerator {
	def main (args :Array[String]): Unit = {
		val gen = new UniverseGenerator
		val universe = gen.generateUniverse()
	}
}