package arx.axistential.game.data.world

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/8/14
 * Time: 1:53 PM
 */

import arx.core.vec.coordinates.VoxelCoord
import arx.engine.data.TWorldAuxData
import arx.tmp.game.logic.datastructures.GenericTalea
import arx.tmp.game.logic.datastructures.GenericTaleaGrid

@SerialVersionUID(1L)
class ConstructionData extends TWorldAuxData {
	val support = new GenericTaleaGrid[Short,GenericTalea[Short]](1000.toShort,(v:VoxelCoord) => new GenericTalea[Short] (v,1000.toShort ))
}
