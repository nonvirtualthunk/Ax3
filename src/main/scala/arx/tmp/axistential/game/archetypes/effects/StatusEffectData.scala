package arx.axistential.game.archetypes.effects

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 9/21/14
 * Time: 4:23 PM
 */

import arx.core.representation.InformationLevel
import arx.core.representation.InformationLevel.InformationLevel
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.data.TGameEntityAuxData

@SerialVersionUID(1L)
class StatusEffectData extends TGameEntityAuxData {
	var statusEffects = List[StatusEffect]()
	def hasStatus (statusArch : StatusEffectArchetype) = statusEffects.exists (s => s.archetype == statusArch)

	override def informationPairs(level: InformationLevel) = Map()
}

object StatusEffectData {
	implicit def entToStatusEffectData (ge : GameEntity) = ge.aux[StatusEffectData]
}