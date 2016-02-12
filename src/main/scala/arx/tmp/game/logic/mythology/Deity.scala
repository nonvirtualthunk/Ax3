package arx.tmp.game.logic.mythology

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/3/14
 * Time: 5:41 PM
 */

import arx.Prelude._
import arx.core.representation.ConfigValue
import arx.core.representation.TConfigurable
import arx.core.traits.ArxEnum
import arx.core.traits.ArxEnumObject
import arx.core.traits.TSentinel
import arx.tmp.game.logic.entities.core.ConfigurableEntityAssistant
import arx.tmp.game.logic.mythology.DeityDescriptors.DeityWithDomainDescriptor
import arx.resource.ResourceManager

class Deity extends MythologicalEntity with TConfigurable {
	this.kind = "deity"
	var rank = Moddable(Deity.Rank.Lesser)
	var domains = Moddable(List[Domain]())

	override def setFromSML(sml: ConfigValue, overwrite: Boolean): Unit = {
		if (sml.rank.nonEmpty) { rank = Deity.Rank(sml.rank.str) }
		val smlDomains = extractSingularOrPlural(sml,"domain","domains").toList
		if (smlDomains.nonEmpty) {
			domains = smlDomains
				.map (_.str)
				.flatMap (d => Deity.domains.find (dom => dom.name =~= d))
		}

	}

}

object Deity {
	val Sentinel : Deity = new Deity with TSentinel {
		protected def readResolve : Object = Deity.Sentinel
	}

	class Rank protected (name:String, val strength : Int) extends ArxEnum(name) {
		val numDomains = strength
	}
	object Rank extends ArxEnumObject[Rank] {
//		def apply (str : String) = TArxEnum.withKey[Rank](str.toLowerCase,Lesser)

		val Minor = new Rank("Minor",0)
		val Lesser = new Rank("Lesser",1)
		val Greater = new Rank("Greater",2)
		val Major = new Rank("Major",3)
	}


	var titles : List[TitleArchetype] = Nil
	var domains : List[Domain] = Nil

	def DomainRestriction (domainName : String) = new DeityWithDomainDescriptor(domainName :: Nil)

	def load () {
		import ConfigurableEntityAssistant._
		val smlFiles = //ResourceManager.sml("axis/entities/deities/Domains.sml") ::
							ResourceManager.sml("axis/entities/deities/Deities.sml") :: Nil
		for (sml <- smlFiles ; namespace <- sml.fields.values) {
			domains :::= parseAllChildrenOfSMLAs[Domain](namespace.Domains)
			titles :::= parseAllChildrenOfSMLAs[TitleArchetype](namespace.Titles)
		}
	}

	load()

}