package arx.tmp.game.logic.mythology

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/13/14
 * Time: 5:28 PM
 */

import arx.core.representation.ConfigValue
import arx.tmp.game.logic.descriptors.TConfiguredDescriptorParser
import arx.tmp.game.logic.descriptors.TDescriptor
import arx.tmp.game.logic.descriptors.TypedDescriptor

object DeityDescriptors {

	class DeityWithDomainDescriptor(domainNames : List[String]) extends TypedDescriptor[Deity] {
		override def doesMatchTyped(ref: Deity): Boolean = {
			domainNames.forall( domainName => ref.domains.exists(d => d.name =~= domainName) )
		}

		override def exampleMatch: Serializable = Deity.Sentinel
	}
	object DeityWithDomainDescriptor extends TConfiguredDescriptorParser {
		override def parseFromSML(sml: ConfigValue): Option[TDescriptor] = {
			extractSingularOrPlural(sml,"domain","domains").map(_.str).toList match {
				case Nil => None
				case domainList => Some(new DeityWithDomainDescriptor(domainList))
			}
		}
		override def kindStrings = "DeityWithDomain" :: "DeityWithDomains" :: "HasDomain" :: Nil
		override def parseFromString(arg: String): TDescriptor = {
			val domains = arg.split(",").map(_.trim).toList
			new DeityWithDomainDescriptor (domains)
		}
	}


	class DeityWithRank(rank : Deity.Rank, comparison : RelativeComparison) extends TypedDescriptor[Deity] {
		override def doesMatchTyped(deity: Deity): Boolean = {
			comparison.compare(deity.rank.strength,rank.strength)
		}
		override def exampleMatch: Serializable = Deity.Sentinel
	}
	object DeityWithRankGreaterThan extends TConfiguredDescriptorParser {
		override def parseFromSML(sml: ConfigValue): Option[TDescriptor] = None
		override def kindStrings: List[String] = "DivineRankAtLeast" :: Nil
		override def parseFromString(arg: String): TDescriptor = {
			new DeityWithRank(Deity.Rank(arg),GreaterThanOrEqualTo)
		}
	}
	object DeityWithRankLessThan extends TConfiguredDescriptorParser {
		override def parseFromSML(sml: ConfigValue): Option[TDescriptor] = None
		override def kindStrings: List[String] = "DivineRankLessThan" :: "DivineRankLowerThan" :: Nil
		override def parseFromString(arg: String): TDescriptor = {
			new DeityWithRank(Deity.Rank(arg),LessThan)
		}
	}
}
