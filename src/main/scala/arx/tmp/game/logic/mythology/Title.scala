package arx.tmp.game.logic.mythology

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/13/14
 * Time: 8:11 PM
 */

import arx.Prelude._
import arx.core.representation.ConfigValue
import arx.core.representation.TConfigurable
import arx.core.traits.TSentinel
import arx.tmp.game.logic.descriptors.ConfiguredDescriptor
import arx.tmp.game.logic.descriptors.TDescriptor
import arx.tmp.game.logic.entities.core.GameArchetype
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.mythology.DeityDescriptors.DeityWithDomainDescriptor
import arx.tmp.game.procedural.writing.ProceduralWriting

class Title extends GameEntity {
	var _archetype = TitleArchetype.Sentinel
	override def archetype = _archetype
	def archetype_= (a : TitleArchetype) { _archetype = a }
	def titleString = name
	def titleString_=(str:String) { name = str }

	var definiteTitleString = ""
}
object Title {
	val Sentinel : Title = new Title with TSentinel {
		name = "Sentinel Title"
		definiteTitleString = "Definite Sentinel Title"
		protected def readResolve : Object = Title.Sentinel
	}
}

class TitleArchetype extends GameArchetype with TConfigurable {
	protected val _template = memoize((s:String) => ProceduralWriting.compileTemplate(s))
	def template = _template(name)
	val forDomain : Option[Domain] = None


	def titleString (forEnt : MythologicalEntity) = {
		var auxMap = Map[String,String]()
		forDomain match {
			case Some(dom) => auxMap += "Domain" -> dom.name
			case _ =>
		}
		ProceduralWriting.generateFromTemplate(template,forEnt,auxMap)
	}

	/** Title string with an appropriate prefix, if needed, to make it a definite form. I.e. the title
	  * "Lord of Storms" would become "the Lord of Storms", but "He whose Body is Stone" would remain
	  * unchanged since it is already a definite form.
	  */
	def definiteTitleString (forEnt : MythologicalEntity) = {
		val nameLC = name.toLowerCase
		if (nameLC.startsWith("[pronoun]") || nameLC.startsWith("the")) {
			titleString(forEnt)
		} else {
			"the " + titleString(forEnt)
		}
	}

	var conditions : List[TDescriptor] = Nil

	override def setFromSML(sml: ConfigValue, overwrite: Boolean): Unit = {
		name = sml.title.strOrElse(name)
		val requiredDomains = extractSingularOrPlural(sml,"domain","domains")
		if (requiredDomains.nonEmpty) {
			conditions ::= new DeityWithDomainDescriptor(requiredDomains.map(_.str).toList)
		}
		for (condition <- extractSingularOrPlural(sml,"condition","conditions")) {
			conditions ::= ConfiguredDescriptor(condition)
		}
	}

	def createTitle (forEnt : MythologicalEntity) = {
		val title = new Title
		title.titleString = titleString(forEnt)
		title.definiteTitleString = definiteTitleString(forEnt)
		title
	}
}
object TitleArchetype {
	val Sentinel : TitleArchetype = new TitleArchetype with TSentinel {
		name = "Titleless"
		protected def readResolve : Object = TitleArchetype.Sentinel
	}
}