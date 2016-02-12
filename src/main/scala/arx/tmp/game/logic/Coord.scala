package arx.tmp.game.logic

import arx.core.vec.ReadVec2i
import arx.core.vec.ReadVec3i
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.Talea

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 6/19/11
 * Time: 11:18 AM
 * Created by nonvirtualthunk
 */

object Coord {
	def worldCoordinateToTaleaCoordinate(v: ReadVec3i): ReadVec3i = { w2t(v) }
	def taleaCoordianteToWorldCoordinate(v: ReadVec3i): ReadVec3i = { t2w(v) }
	def w2t(v: ReadVec3i): ReadVec3i = { v >> Talea.dimensionPo2 }
	def t2w(v: ReadVec3i): ReadVec3i = { v << Talea.dimensionPo2 }
	def t2w_v(v: VoxelCoord): VoxelCoord = { VoxelCoord(v.x << Talea.dimensionPo2,v.y << Talea.dimensionPo2,v.z << Talea.dimensionPo2) }
	def w2t(v: ReadVec2i): ReadVec2i = { v >> Talea.dimensionPo2 }
	def t2w(v: ReadVec2i): ReadVec2i = { v << Talea.dimensionPo2 }
	def w2t(v: Int): Int = { v >> Talea.dimensionPo2 }
	def t2w(v: Int): Int = { v << Talea.dimensionPo2 }
	def w2w(v: ReadVec3i): ReadVec3i = { (v >> Talea.dimensionPo2) << Talea.dimensionPo2 }
	def w2w(v: Int): Int = { (v >> Talea.dimensionPo2) << Talea.dimensionPo2 }
}