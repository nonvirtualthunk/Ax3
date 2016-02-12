package arx.axistential.game.data.entity

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/1/15
 * Time: 10:07 AM
 */

import arx.application.Noto
import arx.axistential.game.archetypes.Skill
import arx.core.representation.InformationLevel
import arx.tmp.game.logic.descriptors.SpecificArchetypeDescriptor
import arx.tmp.game.logic.descriptors.TDescriptor
import arx.tmp.game.logic.entities.data.TGameEntityAuxData

class SkillData extends TGameEntityAuxData {
	var skillXP = Map[Skill,Float]()
	var rawSkillLevels = Map[Skill,Int]()
	var skillLevels = Moddable(() => rawSkillLevels)

	/**
	 * Finds the skill with the highest level of those that match the given descriptor
	 * and returns its level.
 *
	 * @param descriptor the descriptor to match
	 * @return the maximum level among the skills that match the given descriptor
	 */
	def maxSkillLevel( descriptor : TDescriptor ) = {
		var max = 0
		val rskills = skillLevels.resolve()
		rskills.foreach{  case(k,v) => if ( descriptor.matches(k) ) { max = math.max(max,v) } }
		max
	}

	/**
	 * Returns the current level of the specified skill, or 0 if that skill is not known
 *
	 * @param skillArchetype the skill archetype to retrieve level information for
	 * @return the current level of the specified skill, or 0 if that skill is not known
	 */
	def skillLevel( skillArchetype : Skill ) : Int = skillLevels.getOrElse(skillArchetype,0)

	/**
	 * Returns the current maximum level among skills matching the specified descriptor,
	 * if none match, it will return 0
 *
	 * @param skillDescriptor the descriptor to match against
	 * @return maximum level of matching skills or 0 if none match
	 */
	def skillLevel( skillDescriptor : TDescriptor ) : Int = skillDescriptor match {
		case sad : SpecificArchetypeDescriptor => sad.archetype match {
			case skill : Skill => skillLevels.getOrElse(skill,0)
			case other => Noto.warn(s"skillLevel(...) was passed a non-skill archetype $other");0
		}
		case sd => maxSkillLevel(sd)
	}

	/**
	 * @param skillArchetype the skill archetype to retrieve level information for
	 * @return the effective level for the given skill, accounting for synergies
	 */
	def effectiveSkillLevel ( skillArchetype : Skill ) : Int = {
		var lvl = skillLevel(skillArchetype)
		for ( syn <- skillArchetype.receivesSynergyFrom ) {
			val synLevel = skillLevel(syn)
			lvl += synLevel / 4
		}
		lvl
	}

	/**
	 * @param skillDescriptor the skill descriptor to retrieve level information for
	 * @return the effective level for the given skill, accounting for synergies
	 */
	def effectiveSkillLevel ( skillDescriptor : TDescriptor ) : Int = {
		var max = 0
		val rskills = skillLevels.resolve()
		rskills.foreach{  case(k,v) => if ( skillDescriptor.matches(k) ) { max = math.max(max,effectiveSkillLevel(k)) } }
		max
	}

	/**
	 * @param skillWeights the skill weights to determine the effective level for
	 * @return the effective level for the given skill usages, accounting for synergies
	 */
	def effectiveSkillLevel ( skillWeights : List[(Skill,Float)] ) : Int = {
		var sum = 0.0f
		var counter = 0.0f
		for ( (skill,weight) <- skillWeights ) {
			val synLevel = effectiveSkillLevel(skill)
			sum += synLevel * weight
			counter += weight
		}
		if ( counter != 0.0f ) { (sum / counter).round }
		else { 0 }
	}

	override def informationPairs(level : InformationLevel.InformationLevel) : Map[String,Any] = Map()
}
