package arx.tmp.game.logic.entities.data

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/13/14
 * Time: 5:08 PM
 */

import arx.core.representation.ConfigValue
import arx.core.traits.ArxEnum
import arx.core.traits.ArxEnumObject
import arx.tmp.game.logic.descriptors.TConfiguredDescriptorParser
import arx.tmp.game.logic.descriptors.TDescriptor
import arx.tmp.game.logic.descriptors.TEntityDescriptor
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.networking.TNetworkedGameEntityAuxData
import arx.tmp.game.procedural.writing.THasProcWritingTokens
import arx.macros.NetworkedAuxData

class Sex(name:String) extends ArxEnum(name) {
	var pronoun = ""
	var reflexive = ""
	var possessive = ""
	var accusative = ""
	var sibling = ""
	var parent = ""
	var child = ""
	var spouse = ""
	var nuptial = ""
	var siblingInLaw = ""
	var childInLaw = ""


	def writingTokens = Map(
		"pronoun" -> pronoun,
		"reflexive" -> reflexive,
		"possessive" -> possessive,
		"accusative" -> accusative,
		"sibling" -> sibling,
		"parent" -> parent,
		"child" -> child,
		"spouse" -> spouse,
		"nuptial" -> nuptial,
		"siblingInLaw" -> siblingInLaw,
		"childInLaw" -> childInLaw
	)
}
object Sex extends ArxEnumObject[Sex] {
	val Male = Sex("Male")
	val Female = Sex("Female")
	val Neuter = Sex("Neuter")

	Male.pronoun = "he"
	Male.reflexive = "himself"
	Male.possessive = "his"
	Male.accusative = "him"
	Male.parent = "father"
	Male.sibling = "brother"
	Male.child = "son"
	Male.spouse = "husband"
	Male.nuptial = "groom"
	Male.siblingInLaw = "uncle"
	Male.childInLaw = "nephew"

	Female.pronoun = "she"
	Female.reflexive = "herself"
	Female.possessive = "her"
	Female.accusative = "her"
	Female.parent = "mother"
	Female.sibling = "sister"
	Female.child = "daughter"
	Female.spouse = "wife"
	Female.nuptial = "bride"
	Female.siblingInLaw = "aunt"
	Female.childInLaw = "niece"

	Neuter.pronoun = "it"
	Neuter.reflexive = "itself"
	Neuter.possessive = "its"
	Neuter.accusative = "it"
	Neuter.parent = "parent"
	Neuter.sibling = "sibling"
	Neuter.child = "child"
	Neuter.spouse = "spouse"
	Neuter.nuptial = "wedded"
	Neuter.siblingInLaw = "inlaw"
	Neuter.childInLaw = "cousin"
}

@NetworkedAuxData
class SexData extends TNetworkedGameEntityAuxData with THasProcWritingTokens with TConfigurableGameEntityAuxData {
	var sex : Sex = Sex.Neuter

	override def writingTokens: Map[String, String] = sex.writingTokens

	override def createFromSML(sml: ConfigValue): Option[SexData] = if (sml.sex.nonEmptyValue) {
		val SD = new SexData
		SD.sex = Sex(sml.sex.str)
		Some(SD)
	} else { None }
}

@SerialVersionUID(1L)
class EntityWithSexDescriptor(sex : Sex) extends TEntityDescriptor {
	override def matchesEntity(gameEntity: GameEntity): Boolean = {
		gameEntity match {
			case t : GameEntity => t.auxDataOpt[SexData].exists(d => d.sex == sex)
			case _ => false
		}
	}

	override def exampleMatch: GameEntity = GameEntity.Sentinel
}

@SerialVersionUID(1L)
object EntityWithSexDescriptor extends TConfiguredDescriptorParser {
	override def parseFromSML(sml: ConfigValue): Option[TDescriptor] = {
		if (sml.sex.nonEmpty) {
			Some(parseFromString(sml.sex.str))
		} else {
			None
		}
	}
	override def kindStrings = "EntityWithSex" :: "Sex" :: Nil
	override def parseFromString(arg: String): TDescriptor = {
		new EntityWithSexDescriptor (Sex (arg))
	}
}