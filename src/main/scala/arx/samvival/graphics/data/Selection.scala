package arx.samvival.graphics.data

import arx.ai.search.Path
import arx.core.vec.coordinates.AxialVec3
import arx.engine.graphics.data.TGraphicsData
import arx.engine.lworld.LEntity
import arx.samvival.game.actions.GameAction

class Selection extends TGraphicsData  {
	var selectedCharacters = Set[LEntity]()

	protected var _activePaths = Map[LEntity, Path[AxialVec3]]()
	var activePathRevision = 0
	def activePaths = _activePaths
	def activePaths_=(p : Map[LEntity, Path[AxialVec3]]): Unit = {
		_activePaths = p
		activePathRevision += 1
	}

	var selectedPossibleActions : List[GameAction] = Nil
}