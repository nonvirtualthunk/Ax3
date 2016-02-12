package arx.axistential.game.archetypes.effects

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 11/5/12
 * Time: 4:07 PM
 * Created by nonvirtualthunk
 */

import arx.axistential.game.archetypes.effects.StatusEffectData._
import arx.axistential.game.entities.CreatureEntity
import arx.tmp.game.logic.entities.core.GameArchetype

trait StatusEffectArchetype extends GameArchetype {
	/** Short description for the character this is applied to. For example "tired" in the
	  * case of a sleep deprivation, or "hungry" in the case of low calories, or the like */
	def descriptor : String

	final def applyToCreature(creature : CreatureEntity) = {
		val statusEffect = new StatusEffect()
		statusEffect.archetype = this
		statusEffect.name = this.instanceName
		statusEffect.created = creature.worldTime
		statusEffect._appliedCharacter = Some(creature)
		creature.statusEffects ::= statusEffect
		onApplyToCreature(statusEffect, creature)
		statusEffect
	}

	/** Called when the status effect is first applied, this is the place to create the modifiers that will
	  * perform the status effect's effects */
	protected def onApplyToCreature(affliction: StatusEffect, bp: CreatureEntity)

	final def removeFromCreature(statusEffect: StatusEffect, bp: CreatureEntity) {
		statusEffect._appliedCharacter = None
		bp.statusEffects = bp.statusEffects.filterNot( _ eq statusEffect )
		onRemoveFromCharacter(statusEffect, bp)
		statusEffect.removeFromWorld()
	}

	/** Removes all status effects that have this archetype (if any) from the provided
	  * character, and performs all necessary bookkeeping
	 * @param character the character to remove status effects from
	 */
	def removeFromCreature(character : CreatureEntity) {
		character.statusEffects.filter(_.archetype == this).foreach(e => removeFromCreature(e,character))
	}

	/** Called when the status effect is healed or otherwise removed. This does <strong>not</strong> have
	  * to deal with the mechanics of removing modifiers, that will be handled automatically. */
	protected def onRemoveFromCharacter(affliction: StatusEffect, bp: CreatureEntity)
}