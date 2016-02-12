package arx.axistential.game.data.world

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/25/13
 * Time: 2:01 PM
 */

import arx.core.vec.coordinates.VoxelCoord
import arx.engine.data.TWorldAuxData
import arx.tmp.game.logic.datastructures.ByteTalea
import arx.tmp.game.logic.datastructures.GenericRawTaleaGrid
import arx.tmp.game.logic.datastructures.GenericTalea

class GranularData extends TWorldAuxData {
	val levelGrid = new GenericRawTaleaGrid[Byte,GenericTalea[Byte]](0,(v:VoxelCoord) => new GenericTalea[Byte] (v,0 ))
	val materialGrid = new GenericRawTaleaGrid[Byte,ByteTalea](0,(v:VoxelCoord) => new ByteTalea (v,0 ))

	val maxLevel = Byte.MaxValue
	def minLevel = 0

	@inline
	def definedAt ( v : VoxelCoord ) = levelGrid.definedAt(v)
	@inline
	def definedAt ( x : Int, y : Int, z : Int ) = levelGrid.definedAt(x,y,z)
}
