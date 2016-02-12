package arx.axistential.game.logic.general

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/1/15
 * Time: 12:19 PM
 */

import arx.Prelude._
import arx.axistential.game.archetypes.Skill
import arx.axistential.game.data.entity.SkillData
import arx.tmp.game.logic.entities.core.GameEntity

trait TExperienceLogic {
	/**
	 * @return the maximum attainable level
	 */
	def maxLevel : Int

	/**
	 * @return the standard scale for experience, usually, the number of xp
	 *         required to move from level 0 to level 1 under standard
	 *         conditions, all xp gain should be defined in terms of this
	 */
	def baseXPScale : Float

	/**
	 * @param level the level being attained
	 * @return the total amount of experience required to reach the given level
	 */
	def xpForLevel ( level : Int ) : Float

	/**
	 * Increase the amount of experience the given entity has, adjusting its
	 * level as necessary
	 * @param ent the entity to give experience to
	 * @param xpGain the amount of experience to gain
	 * @return whether or not the experience gain resulted in a level change
	 */
	def gainXP ( ent : GameEntity, skill : Skill, xpGain : Float ) : Boolean = {
		val skillData = ent.aux[SkillData]
		val newXP = skillData.skillXP.getOrElse(skill,0.0f) + xpGain
		skillData.skillXP += skill -> newXP
		var ret = false
		while ( newXP >= xpForLevel(skillData.rawSkillLevels.getOrElse(skill,0)+1) ) {
			skillData.rawSkillLevels += (skill -> (skillData.rawSkillLevels.getOrElse(skill,0)+1))
			ret = true
		}
		ret
	}

	/**
	 * The base multiplier that a given level should provide to a task that uses
	 * it, for example speed of completing an action, or average quality of a
	 * produced item. This can control, for example, whether levels have a
	 * linear, exponential, or logarithmic improvement on performance
	 * @param level the level to get the base multiplier for
	 * @return the multiplier, expressed in terms of (0.0f,1.0f,N) with a higher
	 *         value always being better, and 1.0f being the baseline for
	 *         no particular skill
	 */
	def baseMultiplierForLevel ( level : Int ) : Float

	/**
	 * Train one of a character's skills, with the amount scaled to <code>1/numPerBaseLevel</code> multiplied
	 * by the amount of experience needed to advance to first level.
	 * @param ent the entity to train a skill for
	 * @param skill skill to train
	 * @param numPerBaseLevel number of events (like this one) that would need to happen to train from
	 *                        level 0 to level 1, scales the speed of advancement
	 * @param thisIncrement what percent of an event this one is, can be useful for skills that train more continuously
	 *                      instead of discretely
	 * @return <code>true</code> if a level was gained, <code>false</code> otherwise
	 */
	def trainSkill ( ent : GameEntity , skill : Skill, numPerBaseLevel : Int , thisIncrement : Float = 1.0f ) : Boolean
}
class DefaultExperienceLogic extends TExperienceLogic {
	def maxLevel = 100
	val xpForLevelX = (for ( i <- 0 to maxLevel ) yield {
		((i * (i + 1)) / 2) * baseXPScale
		//0  1  2  3  4  5  6  7  8  9  10
		//0  1  3  6  10 15 21 28 36 45 55
	}).toArray
	def baseXPScale = 100.0f
	def xpForLevel(i: Int) = if ( i >= 0 && i < maxLevel ) { xpForLevelX(i) } else if (i < 0) { 0.0f } else { 1000000000.0f }

	def baseMultiplierForLevel(level: Int) = if ( level >= 0 ) {
		1.0f + 0.5f * (level - 1).toFloat
	} else {
		math.max(0.00001f,1.0f * powf(0.5f,level.toFloat)) //prevent it ever giving out a 0
	}


	def trainSkill(ent: GameEntity, skill: Skill, numPerBaseLevel: Int, thisIncrement : Float = 1.0f) = {
		val ratio = 1.0f / math.max(1.0f,numPerBaseLevel.toFloat)
		val xp = ratio * baseXPScale * thisIncrement
		gainXP(ent,skill,xp)
	}
}

object ExperienceLogic {
	protected val impl = pio[TExperienceLogic]
	/**
	 * @return the maximum attainable level
	 */
	def maxLevel : Int = impl.maxLevel

	/**
	 * @return the standard scale for experience, usually, the number of xp
	 *         required to move from level 0 to level 1 under standard
	 *         conditions, all xp gain should be defined in terms of this
	 */
	def baseXPScale : Float = impl.baseXPScale

	/**
	 * @param level the level being attained
	 * @return the total amount of experience required to reach the given level
	 */
	def xpForLevel ( level : Int ) : Float = impl.xpForLevel(level)

	/**
	 * Increase the amount of experience the given entity has, adjusting its
	 * level as necessary
	 * @param ent the entity to give experience to
	 * @param xpGain the amount of experience to gain
	 * @return whether or not the experience gain resulted in a level change
	 */
	def gainXP ( ent : GameEntity, skill : Skill, xpGain : Float ) : Boolean = impl.gainXP(ent,skill,xpGain)

	/**
	 * The base multiplier that a given level should provide to a task that uses
	 * it, for example speed of completing an action, or average quality of a
	 * produced item. This can control, for example, whether levels have a
	 * linear, exponential, or logarithmic improvement on performance
	 * @param level the level to get the base multiplier for
	 * @return the multiplier, expressed in terms of (0.0f,1.0f,N) with a higher
	 *         value always being better, and 1.0f being the baseline for
	 *         no particular skill
	 */
	def baseMultiplierForLevel ( level : Int ) : Float = impl.baseMultiplierForLevel(level)

	/**
	 * Train one of a character's skills, with the amount scaled to <code>1/numPerBaseLevel</code> multiplied
	 * by the amount of experience needed to advance to first level.
	 * @param ent the entity to train a skill for
	 * @param skill skill to train
	 * @param numPerBaseLevel number of events (like this one) that would need to happen to train from
	 *                        level 0 to level 1, scales the speed of advancement
	 * @param thisIncrement what percent of an event this one is, can be useful for skills that train more continuously
	 *                      instead of discretely
	 * @return <code>true</code> if a level was gained, <code>false</code> otherwise
	 */
	def trainSkill ( ent : GameEntity , skill : Skill, numPerBaseLevel : Int , thisIncrement : Float = 1.0f ) : Boolean = impl.trainSkill(ent,skill,numPerBaseLevel,thisIncrement)
}