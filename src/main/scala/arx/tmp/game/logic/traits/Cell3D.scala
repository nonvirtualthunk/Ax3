package arx.tmp.game.logic.traits

import arx.core.vec.ReadVec3i
import arx.core.vec.coordinates.VoxelCoord


/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/16/11
 * Time: 4:00 PM
 * To change this template use File | Settings | File Templates.
 */

trait Cell3D extends Cell {
	def position: VoxelCoord
	def shiftedPosition: ReadVec3i

	def x: Int = position.x
	def y: Int = position.y
	def z: Int = position.z
}