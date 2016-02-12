package arx.axistential.game.archetypes.effects

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 9/22/14
 * Time: 7:50 AM
 */

import arx.axistential.game.entities.CreatureEntity
import arx.axistential.modules.hunger.MetabolicData
import arx.core.modifiers.CommonModifiers

object Unconscious extends StatusEffectArchetype {
	override def descriptor = "unconscious"

	override protected def onRemoveFromCharacter(affliction: StatusEffect, bp: CreatureEntity) = {
		// no need
	}

	override protected def onApplyToCreature(affliction: StatusEffect, bp: CreatureEntity) = {
		// a creature can perform no actions while unconscious
		bp.actionRate = affliction.createModifier(bp.actionRate,(r:Float) => 0.0f)
		val MD = bp.aux[MetabolicData]
		MD.baseMetabolicRate = affliction.createModifier(MD.baseMetabolicRate,CommonModifiers.MultiplyByConstant(0.5f))
	}
}
