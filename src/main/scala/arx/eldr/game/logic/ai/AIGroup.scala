package arx.eldr.game.logic.ai

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.engine.data.TGameEntityAuxData
import arx.engine.entity.GameEntity
import arx.engine.entity.TGameEntity
import scalaxy.loops._

//class AIGroup extends GameEntity {
//	var entities : Set[GameEntity] = Set()
//	var goals : List[Goal] = Nil
//}

class AIGroupData extends TGameEntityAuxData {
	var entities : Set[TGameEntity] = Set()
	var goals : Set[Goal] = Set()
	var finishedGoals : Set[Goal] = Set()
	var inProgressGoals : Set[Goal] = Set()
}