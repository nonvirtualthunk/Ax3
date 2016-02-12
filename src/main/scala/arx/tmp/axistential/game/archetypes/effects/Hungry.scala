package arx.axistential.game.archetypes.effects

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 11/5/12
 * Time: 4:18 PM
 * Created by nonvirtualthunk
 */

import arx.axistential.game.entities.CreatureEntity
import arx.axistential.modules.hunger.MetabolicData
import arx.core.modifiers.CommonModifiers

class HungerArchetype(name:String,slowDownTo:Float,metabolismMult:Float) extends StatusEffectArchetype {
	/** Short description for the character this is applied to. For example "tired" in the
	  * case of a sleep deprivation, or "hungry" in the case of low calories, or the like */
	def descriptor = name

	/** Called when the status effect is first applied, this is the place to create the modifiers that will
	  * perform the status effect's effects */
	protected def onApplyToCreature(affliction: StatusEffect, bp: CreatureEntity) {
		//Halve the number of action points the hungry character gets
		bp.actionRate = affliction.createModifier(bp.actionRate,CommonModifiers.MultiplyByConstant(slowDownTo))
		if (metabolismMult != 1.0f) {
			val MD = bp.aux[MetabolicData]
			MD.baseMetabolicRate = affliction.createModifier(MD.baseMetabolicRate,CommonModifiers.MultiplyByConstant(metabolismMult))
		}
	}

	/** Called when the status effect is healed or otherwise removed. This does <strong>not</strong> have
	  * to deal with the mechanics of removing modifiers, that will be handled automatically. */
	protected def onRemoveFromCharacter(affliction: StatusEffect, bp: CreatureEntity) {
		//Nothing to do here really, assuming that the modifiers we've created are auto-removed
	}
}
object Hungry extends HungerArchetype("hungry",0.8f,1.0f)
object VeryHungry extends HungerArchetype("very hungry",0.6f,1.0f)
object Starving extends HungerArchetype("starving",0.3f,0.8f)

object WellFed extends HungerArchetype("well fed",1.1f,1.05f)