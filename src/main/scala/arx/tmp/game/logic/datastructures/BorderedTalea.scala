package arx.tmp.game.logic.datastructures

import arx.core.vec.coordinates.VoxelCoord

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/22/11
 * Time: 10:00 AM
 * Created by nonvirtualthunk
 */

@SerialVersionUID(1L)
class BorderedTalea[T](pos: VoxelCoord,_defaultValue: T)(implicit man : Manifest[T]) extends GenericTalea[T](pos,_defaultValue)(man) with TInfiniteVoxelStore[T]{

	override def apply ( x: Int, y : Int, z : Int ) : T = {
		if ( nonDefaultCount != 0 ) {
			if ( (x >> Talea.dimensionPo2) == 0 && (y >> Talea.dimensionPo2) == 0 && (z >> Talea.dimensionPo2) == 0 ) {
				data(x,y,z)
			} else {
				_defaultValue
			}
		} else {
			_defaultValue
		}
	}

	override def update ( x: Int, y : Int, z : Int , b : T ){
		if ( (x >> Talea.dimensionPo2) == 0 && (y >> Talea.dimensionPo2) == 0 && (z >> Talea.dimensionPo2) == 0 ) {
			super.update(x,y,z,b)
		} else {
			throw new IndexOutOfBoundsException
		}
	}
}

object BorderedTalea {
	def view[T : Manifest,TaleaType <: GenericTalea[T]] ( t: TaleaType , borderValue : T ) : BorderedTaleaViewer[T,TaleaType] = {
		new BorderedTaleaViewer(t,borderValue)
	}
}