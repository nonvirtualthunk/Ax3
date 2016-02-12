package arx.axistential.game.data.world

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/7/13
 * Time: 9:08 AM
 */

import arx.axistential.game.world.generation.generators.geological.GeologicalFeature
import arx.engine.data.TWorldAuxData
import arx.tmp.game.logic.datastructures.CellGrid2D

@SerialVersionUID(1L)
class GeologicalFeatureData extends TWorldAuxData with Serializable {
	val step = 64
	val geologicalFeatures = CellGrid2D[GeologicalFeature](step)
	var allFeatures : List[GeologicalFeature] = Nil
}