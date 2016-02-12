package arx.tmp.game.logic.lighting

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/21/13
 * Time: 8:33 AM
 * Created by nonvirtualthunk
 */

import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.GenericTalea
import arx.tmp.game.logic.datastructures.Talea

class ShadowGrid extends TShadowGrid {
	val taleae = Array.ofDim[GenericTalea[Short]](64)
	def getTalea ( xi : Int , yi : Int, zi : Int ) = taleae( (xi<<4) + (yi<<2) + zi )

	for ( i <- 0 until 64 ) { taleae(i) = new GenericTalea[Short](VoxelCoord(0,0,0),0.toShort) }
	private val ANDWith = Talea.dimensionM1
	private val ShiftBy = Talea.dimensionPo2
	private val offset = Talea.dimension << 1

	def apply ( x : Int, y : Int, z : Int ) : Float = {
		val talea = getTalea( (x + offset) >> ShiftBy , (y + offset) >> ShiftBy , (z + offset) >> ShiftBy )
		talea((x & ANDWith),(y & ANDWith),(z & ANDWith)) * 1.0e-4f
	}

	def update ( x : Int, y : Int, z : Int , f : Float ) {
		val talea = getTalea( (x + offset) >> ShiftBy , (y + offset) >> ShiftBy , (z + offset) >> ShiftBy )
		talea((x & ANDWith),(y & ANDWith),(z & ANDWith)) = (f * 10000).toShort
	}
}