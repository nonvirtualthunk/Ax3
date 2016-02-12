package arx

import arx.core.vec.coordinates.VoxelCoord

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/8/13
 * Time: 2:37 PM
 * To change this template use File | Settings | File Templates.
 */
package object axistential {
	def allCardinalAdjacents ( v : VoxelCoord ) = v.minusX(1) :: v.minusY(1) :: v.minusZ(1) ::
		v.plusX(1) :: v.plusY(1) :: v.plusZ(1) :: Nil

	def allXYCardinalAdjacents ( v : VoxelCoord ) = v.minusX(1) :: v.minusY(1) ::
		v.plusX(1) :: v.plusY(1) :: Nil

	def allXYAdjacents ( v : VoxelCoord ) = allXYCardinalAdjacents(v) ::: VoxelCoord(v.x+1,v.y+1,v.z) :: VoxelCoord(v.x+1,v.y-1,v.z) ::
		VoxelCoord(v.x-1,v.y+1,v.z) :: VoxelCoord(v.x-1,v.y-1,v.z) :: Nil
}
