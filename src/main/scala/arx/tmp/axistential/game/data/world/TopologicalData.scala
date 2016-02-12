package arx.axistential.game.data.world

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/6/13
 * Time: 1:12 PM
 */

import arx.engine.data.TWorldAuxData
import arx.tmp.game.logic.datastructures.CellGrid2D

class TopologicalData extends TWorldAuxData {
	val heightmap = CellGrid2D[Int]()
	val depthmap = CellGrid2D[Int]()
}
