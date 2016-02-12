package arx.axistential.game.logic.descriptors

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/2/15
 * Time: 10:01 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.archetypes.Skill
import arx.axistential.game.data.entity.SkillData
import arx.axistential.game.entities.CreatureEntity
import arx.core.representation.ConfigValue
import arx.tmp.game.logic.descriptors.TConfiguredDescriptorParser
import arx.tmp.game.logic.descriptors.TDescriptor
import arx.tmp.game.logic.descriptors.TEntityDescriptor
import arx.tmp.game.logic.entities.core.GameEntity

case class EntityWithSkillDescriptor(skill : Skill,level : Int) extends TEntityDescriptor {
	override def matchesEntity(gameEntity: GameEntity): Boolean = {
		gameEntity.auxDataOpt[SkillData] match {
			case Some(sd) => sd.effectiveSkillLevel(skill) >= level
			case None => false
		}
	}

	override def exampleMatch: GameEntity = CreatureEntity.Sentinel
}

object EntityWithSkillDescriptorParser extends TConfiguredDescriptorParser {
	override def parseFromSML(sml: ConfigValue): Option[TDescriptor] = {
		if (sml.skill.nonEmptyValue) {
			val skill = Skill.withName(sml.skill.str)
			val level = sml.level.intOrElse(0)
			Some(EntityWithSkillDescriptor(skill,level))
		} else {
			None
		}
	}
	override def kindStrings: List[String] = List("EntityWithSkill")
	override def parseFromString(arg: String): TDescriptor = {
		arg.split(" ").toList match {
			case only :: Nil => EntityWithSkillDescriptor(Skill.withName(only),0)
			case skillName :: level :: Nil =>
				level.toIntOpt match {
					case Some(i) => EntityWithSkillDescriptor(Skill.withName(skillName),i)
					case None =>
						Noto.warn(s"Invalid level number provided in EntityWithSkill descriptor: $level")
						EntityWithSkillDescriptor(Skill.withName(skillName),0)
				}
			case _ =>
				Noto.warn(s"Unknown format provided to EntityWithSkill(...) descriptor: $arg")
				TDescriptor.Sentinel
		}
	}
}