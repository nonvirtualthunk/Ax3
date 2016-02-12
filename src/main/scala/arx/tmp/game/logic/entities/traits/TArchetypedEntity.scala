package arx.tmp.game.logic.entities.traits

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/17/13
 * Time: 10:47 AM
 * To change this template use File | Settings | File Templates.
 */

import arx.tmp.game.logic.entities.core.GameArchetype
import arx.tmp.game.logic.entities.core.GameEntity

trait TArchetypedEntity extends GameEntity {
	var _archetype : GameArchetype = GameArchetype.Sentinel
	override def archetype = _archetype
	def archetype_= ( arch : GameArchetype ) { _archetype = arch }
}
