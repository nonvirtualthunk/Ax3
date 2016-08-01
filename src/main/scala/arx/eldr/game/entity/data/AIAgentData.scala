package arx.eldr.game.entity.data

/**
 * TODO: Add javadoc
 */

import arx.eldr.game.logic.ai.Goal
import arx.engine.data.TGameEntityAuxData
import arx.engine.entity.GameEntity
import arx.engine.entity.TGameEntity

class AIAgentData extends TGameEntityAuxData {
	var groups: Set[TGameEntity] = Set()
	var goals: Set[Goal] = Set()
}
