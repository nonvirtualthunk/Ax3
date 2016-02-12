package arx.axistential.game.archetypes.effects

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 9/22/14
 * Time: 8:10 AM
 */

import arx.axistential.game.entities.CreatureEntity

object Dead extends StatusEffectArchetype {
	override def descriptor = "dead"

	override protected def onRemoveFromCharacter(affliction: StatusEffect, bp: CreatureEntity) = {
		// no need
	}

	override protected def onApplyToCreature(affliction: StatusEffect, bp: CreatureEntity) = {
		// a creature can perform no actions while unconscious
		bp.actionRate = affliction.createModifier(bp.actionRate,(r:Float) => 0.0f)
		bp._alive = affliction.createModifier(bp.alive,(isAlive:Boolean) => false)
	}
}