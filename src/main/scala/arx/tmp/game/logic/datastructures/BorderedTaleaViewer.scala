package arx.tmp.game.logic.datastructures

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 9/21/14
 * Time: 2:55 PM
 */

class BorderedTaleaViewer[T : Manifest,TaleaType <: GenericTalea[T]](val wrappedTalea: TaleaType,val borderValue: T) extends TInfiniteVoxelView[T]{
	def apply(x: Int, y: Int, z: Int) : T = {
		if ( (x >> Talea.dimensionPo2) == 0 && (y >> Talea.dimensionPo2) == 0 && (z >> Talea.dimensionPo2) == 0 ) {
			wrappedTalea(x,y,z)
		} else {
			borderValue
		}
	}
}
