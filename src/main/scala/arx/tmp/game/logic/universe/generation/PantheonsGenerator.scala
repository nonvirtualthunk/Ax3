package arx.tmp.game.logic.universe.generation

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/15/14
 * Time: 9:38 AM
 */

import arx.Prelude._
import arx.core.mathutil.WeightedDistribution
import arx.tmp.game.logic.entities.data.Sex
import arx.tmp.game.logic.entities.data.SexData
import arx.tmp.game.logic.mythology.DeityDescriptors.DeityWithDomainDescriptor
import arx.tmp.game.logic.mythology.BasicEventGenerator
import arx.tmp.game.logic.mythology.Deity
import arx.tmp.game.logic.mythology.Pantheon
import arx.tmp.game.logic.universe.data.DeityData
import arx.tmp.game.logic.universe.data.LanguageData
import arx.tmp.game.logic.universe.TUniverseDataGenerator
import arx.tmp.game.logic.universe.Universe

import scalaxy.loops._

class PantheonsGenerator extends TUniverseDataGenerator {
	dependencies ::= classOf[UniverseLanguageGenerator]

	var monotheisticChance = 0.2f
	var polytheisticChance = 0.65f
	var multitheisticChance = 0.1f
	var animisticChance = 0.05f

	val cardinalityList = List(
		monotheisticChance -> Pantheon.Monotheistic,
		polytheisticChance -> Pantheon.Polytheistic,
		multitheisticChance -> Pantheon.Multitheistic,
		animisticChance -> Pantheon.Animistic
	)

	val approximateNumberOfGodsByCardinality = Map(
		Pantheon.Monotheistic -> Map(Deity.Rank.Major -> 1),
		Pantheon.Polytheistic -> Map(Deity.Rank.Major -> 3,Deity.Rank.Greater -> 6,Deity.Rank.Lesser -> 10),
		Pantheon.Multitheistic -> Map(Deity.Rank.Greater -> 3,Deity.Rank.Lesser -> 12,Deity.Rank.Minor -> 20),
		Pantheon.Animistic -> Map(Deity.Rank.Minor -> 100)
	)

	val cardinalityDistribution = WeightedDistribution(cardinalityList)

	def generateDeity(universe : Universe,pantheon: Pantheon) = {
		val deity = new Deity
		deity.aux[SexData].sex = randFrom(List(Sex.Male,Sex.Female))
		deity.name = pantheon.language.generateDeityName(deity.aux[SexData].sex,Map()).personalName
		if (! pantheon.deities.exists(d => new DeityWithDomainDescriptor("trickery"::Nil).matches(d))) {
			deity.domains = List( Deity.domains.find(d => d.name =~= "trickery").get )
		} else {
			deity.domains = List(randFrom(Deity.domains))
		}
		val titleArch = randFrom(Deity.titles.filter(t => t.conditions.forall(c => c.matches(deity))))
		val title = titleArch.createTitle(forEnt = deity)
		deity.titles ::= title

//		deity.name = pantheon.language.generateName()
		deity
	}

	override def generateData(universe: Universe): Unit = {
		val DD = universe.aux[DeityData]

		val numPantheons = rand(randomGenerator,2,6)
		for (i <- 0 until numPantheons) {
			val pantheon = new Pantheon
			pantheon.cardinality = cardinalityDistribution.rand(randomGenerator)
			pantheon.language = randFrom(randomGenerator,universe.aux[LanguageData].languages)

			val approxNumGods = approximateNumberOfGodsByCardinality(pantheon.cardinality)
			val numGodsSortedByRank = approxNumGods.toList.sortBy(_._1.strength).reverse
			for ((rank,median) <- numGodsSortedByRank) {
				val num = ND(randomGenerator,median,median * 0.15f).toInt.max(1)
				for (i <- 0 until num optimized) {
					val deity = generateDeity(universe,pantheon)
					pantheon.deities += deity
				}
			}

			val event = BasicEventGenerator.generateEvent(pantheon)
			DD.pantheons ::= pantheon
		}

		DD.pantheons
	}
}
object PantheonsGenerator {
	type GodRankDistribution = Map[Deity.Rank,Int]
}
