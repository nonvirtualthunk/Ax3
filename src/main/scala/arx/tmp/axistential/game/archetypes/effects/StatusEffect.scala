package arx.axistential.game.archetypes.effects

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 11/5/12
 * Time: 4:05 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.ai.Goal
import arx.axistential.game.entities.CreatureEntity
import arx.core.TModifierSource
import arx.core.units.UnitOfTime
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.traits.TArchetypedEntity

class StatusEffect extends GameEntity with TModifierSource with TArchetypedEntity {
	/** An optional explanation of */
	var causedBy : Option[Cause] = None

	/** Remaining duration for this status effect, if it is duration based,
	  * otherwise will likely be infinite, if it is condition based
	  */
	var remainingDuration: UnitOfTime = foreverTime

	/** When the status effect was applied to the character. Can be used to determined how
	  * long it took to remove, etc.
	  */
	var created: UnitOfTime = 0.seconds

	/** Character to which this applies, used primarily for bookkeeping, should not be accessed
	  * by subclasses, will be <code>None</code> during onRemoveFromBodyPart(...) */
	var _appliedCharacter: Option[CreatureEntity] = None

	/** How long the affliction has been applied to its body part */
	def age: UnitOfTime = this.worldTime - created

	/** Determines whether or not this should remain in effect. Effects can be set to cease on
	  * some end condition, or on a fixed duration */
	def shouldStillApply : Boolean = {
		endCondition(this) && remainingDuration > zeroSeconds
	}
	var endCondition : (StatusEffect) => Boolean = (s) => true
	var endConditionInterval : UnitOfTime = 1.second

	def removeFromCreature() {
		_appliedCharacter match {
			case Some(bp) => this.archetype match {
				case aff: StatusEffectArchetype => aff.removeFromCreature(this, bp)
				case _ => Noto.severeError("Affliction has non-affliction archetype")
			}
			case None => Noto.warn("Removing affliction that has already been removed")
		}
	}
}

@SerialVersionUID(1L)
abstract class Cause extends Serializable {
	def description : String
}
object Cause {
	class ActionByEntity (actingEntity : GameEntity, action : Goal) extends Cause {
		override def description: String = actingEntity.name + " " + action.getClass.getSimpleName
	}
	class ConditionOfEntity (condition : String) extends Cause {
		override def description: String = condition
	}
}