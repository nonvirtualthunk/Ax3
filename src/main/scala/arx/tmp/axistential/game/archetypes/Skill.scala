package arx.axistential.game.archetypes

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/1/15
 * Time: 10:07 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.representation.ConfigValue
import arx.core.representation.TConfigurable
import arx.core.traits.TSentinel
import arx.tmp.game.logic.entities.core.ConfigurableEntityAssistant
import arx.tmp.game.logic.entities.core.GameArchetype
import arx.resource.ResourceManager

class Skill extends GameArchetype with TConfigurable {
	protected var receivesSynergyFromLookups = Set[String]()
	protected var _receivesSynergyFrom = Set[Skill]()
	def receivesSynergyFrom = {
		for (lookup <- receivesSynergyFromLookups) {
			Skill.skillsByName.get(lookup) match {
				case Some(otherSkill) =>
					otherSkill._receivesSynergyFrom += this
					this._receivesSynergyFrom += otherSkill
					receivesSynergyFromLookups -= lookup
				case _ =>
			}
		}
		_receivesSynergyFrom
	}

	var practitionerName = "Skiller"

	override def setFromSML(sml: ConfigValue, overwrite: Boolean): Unit = {
		name = sml.name.strOrElse(name)
		practitionerName = sml.practitionerName.strOrElse(practitionerName)
		for (v <- sml.synergizesWith.arr; synergyTarget = v.str) {
			Skill.skillsByName.get(synergyTarget) match {
				case Some(otherSkill) =>
					otherSkill._receivesSynergyFrom += this
					this._receivesSynergyFrom += otherSkill
				case _ =>
					receivesSynergyFromLookups += synergyTarget
			}
		}

	}
}

object Skill {
	val Sentinel : Skill = new Skill with TSentinel {
		name = "Sentinel Skill"
	}
	var skills = List[Skill]()
	protected var skillsByName = Map[CaseInsensitiveString,Skill]()

	def registerSkill (skill : Skill): Unit = {
		skills ::= skill
		skillsByName += (skill.name : CaseInsensitiveString) -> skill
	}

	def withName (str : String) = skillsByName.getOrElse(str,{
		Noto.warn(s"No skill with name $str, returning sentinel")
		Skill.Sentinel
	})


	protected def load (): Unit = {
		val sml = ResourceManager.sml("axis/entities/skills")
		for ((namespace,contents) <- sml.fields) {
			skills :::= ConfigurableEntityAssistant.parseAllChildrenOfSMLAs[Skill](contents)
		}
		skillsByName = skills.map(a => (a.name : CaseInsensitiveString) -> a).toMap
	}
	load()
}