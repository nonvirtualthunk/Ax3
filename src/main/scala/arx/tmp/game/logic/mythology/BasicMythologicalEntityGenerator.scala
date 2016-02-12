package arx.tmp.game.logic.mythology

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/5/14
 * Time: 1:07 PM
 */

import arx.core.language.semantic.Grammar.PartOfSpeech._
import arx.core.language.semantic.Grammar.DefiniteForm
import arx.core.language.semantic.Grammar.PositiveAdjective
import arx.core.language.semantic.Grammar.WordForm
import arx.core.language.semantic._
import arx.tmp.game.logic.descriptors.TDescriptor
import arx.tmp.game.logic.entities.data.Sex
import arx.tmp.game.logic.mythology.descriptors.MythologicalPlaceDescriptor
import arx.tmp.game.logic.universe.Universe
import arx.tmp.game.logic.universe.data.DeityData
import arx.tmp.game.logic.universe.data.LanguageData

import scala.util.Random

object BasicMythologicalEntityGenerator extends TMythologicalEntitySubGenerator {
	val dict = SemanticDictionary
	val hall = Map("hall" -> "Hall")

	override def generateEntityWithDescriptor (descriptor : TDescriptor, pantheon : Pantheon): Option[MythologicalEntity] = {
		descriptor match {
			case MythologicalPlaceDescriptor(positive) =>
				val allMeetingPlaces = dict
					.withPartOfSpeech(Noun)
					.withWordKind(WordKind.Place)
					.withLink("isA","meeting place")
				val narrowedMeetingPlaces = if (positive) {
					allMeetingPlaces.withPositivity(_ > 0)
				} else {
					allMeetingPlaces
				}

				val baseType = narrowedMeetingPlaces.rand.nounFormFor(DefiniteForm(singular = true))

				val baseObjects = dict.select (Noun, WordKind.Object, positivity = _ >= 0) ++
					dict.select (partOfSpeech = Noun, wordKind = WordKind.LivingThing, positivity = _ >= 0)
				val ofObject = baseObjects.filter( e => ! e.hasLink("isPartOf") || e.hasProperty("metonymic") ).rand.pluralForm
				val possibleDescriptionAdjectives = dict.select(partOfSpeech = Adjective,positivity = _ >= 0,specificity = _ >= -1)
				val possibleDescriptionVerbs = dict.select(partOfSpeech = Verb,positivity = _ >= 0,specificity = _ >= -1,quantity = ("poetic",_ >= 0))
					.filter ( _.isRoot )

				val possibleDescriptions = (possibleDescriptionAdjectives ++ possibleDescriptionVerbs).filterNot(_.rootEntry == ofObject.rootEntry)
				val descWord = possibleDescriptions.rand.adjectiveForm(PositiveAdjective)

				val desc = List[Word](baseType,Word("of",EmptySemanticEntry,WordForm.Sentinel),descWord,ofObject)
				println(desc)

				val ent = new MythologicalEntity()
				ent.name = pantheon.language.generatePlaceName(List(descWord,ofObject)) + f" ($baseType of $descWord $ofObject)"
				ent.kind = "place"

				Some(ent)
		}
	}

	def main (args : Array[String]) {
		MathPrelude.random = new Random(System.currentTimeMillis())
//		val ug = new UniverseGenerator
		val universe = Universe.universe//ug.generateUniverse()


		for ( i <- 0 until 20 ) {
			generateEntityWithDescriptor(MythologicalPlaceDescriptor(positive = true),universe.aux[DeityData].pantheons.head)
		}

		for ( i <- 0 until 20 ) {
			println(universe.aux[LanguageData].languages.head.generateDeityName(Sex.Male,Map()))
		}
	}
}
