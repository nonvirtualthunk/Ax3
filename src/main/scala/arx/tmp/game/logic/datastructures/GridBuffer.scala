package arx.tmp.game.logic.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/8/12
 * Time: 11:16 AM
 * Created by nonvirtualthunk
 */

import arx.core.vec.coordinates.VoxelCoord

class GridBuffer[@specialized(Byte,Short,Int) T,TaleaType <: ITalea[T]] ( grid : TTaleaGrid[T,TaleaType] ) {
	private var activeTalea = grid.rawGetTalea(VoxelCoord.Center,readOnly = false)
	private var activeTaleaRO = grid.rawGetTalea(VoxelCoord.Center,readOnly = true)

	private val dPo2 = Talea.dimensionPo2

	def taleaForRO ( x : Int , y : Int , z : Int ) : TaleaType = {
		val cur = activeTaleaRO
		val tDimPo2 = dPo2
		val shiftedPos = cur.shiftedPosition
		if ( ((x >> tDimPo2) != (shiftedPos.x)) ||
			  ((y >> tDimPo2) != (shiftedPos.y)) ||
			  ((z >> tDimPo2) != (shiftedPos.z)) )
		{
			val newT = grid.taleaForReadOnly(x,y,z)
			activeTaleaRO = newT
			newT
		} else {
			cur
		}
	}
	def taleaFor ( x : Int , y : Int , z : Int ) : TaleaType = {
		val cur = activeTalea
		val tDimPo2 = dPo2
		val shiftedPos = cur.shiftedPosition
		if ( ((x >> tDimPo2) != (shiftedPos.x)) ||
			  ((y >> tDimPo2) != (shiftedPos.y)) ||
			  ((z >> tDimPo2) != (shiftedPos.z)) )
		{
			val newT = grid.taleaFor(x,y,z)
			activeTalea = newT
			newT
		} else {
			cur
		}
	}
	def apply ( x : Int , y : Int , z : Int ) : T = {
		val t = taleaForRO(x,y,z)
		t(x - t.x,y - t.y,z - t.z)
	}
	def update ( x : Int , y : Int , z : Int , b : T ) {
		val t = taleaFor(x,y,z)
		t(x - t.x,y - t.y,z - t.z) = b
	}
}

object GridBuffer {
	def apply[@specialized(Byte,Short,Int) T,TaleaType <: ITalea[T]] ( t : TTaleaGrid[T,TaleaType] ) = {
		new GridBuffer(t)
//		t match {
//			case gt : GenericTaleaGrid[T,TaleaType] => new GridBuffer(gt)
//			case gr : GenericRawTaleaGrid[T,TaleaType] => new GridBuffer(gr)
//			case _ => throw new UnsupportedOperationException
//		}
	}
}