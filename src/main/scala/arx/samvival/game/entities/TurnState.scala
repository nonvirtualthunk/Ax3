package arx.samvival.game.entities

import arx.core.macros.GenerateCompanion
import arx.engine.lworld.LEntity

@GenerateCompanion
class TurnState extends SVAuxData {
	var turnNumber = 1
	var activeFaction : LEntity = LEntity.Sentinel
	var factionOrder : List[LEntity] = Nil
}
