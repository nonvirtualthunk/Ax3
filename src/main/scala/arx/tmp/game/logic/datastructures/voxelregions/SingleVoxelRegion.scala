package arx.tmp.game.logic.datastructures.voxelregions

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/23/15
 * Time: 1:20 PM
 */

import arx.core.vec.coordinates.VoxelCoord

class SingleVoxelRegion (voxel : VoxelCoord) extends CubeVoxelRegion(voxel,voxel) {
	override def performIntersection(other: VoxelRegion): VoxelRegion = if (other.contains(voxel)) { this } else { EmptyVoxelRegion }
	override def without(other: VoxelRegion): VoxelRegion = if (other.contains(voxel)) { EmptyVoxelRegion } else { this }
	override def edgeVoxels: VoxelRegion = this
	override def contains(x: Int, y: Int, z: Int): Boolean = x == voxel.x && y == voxel.y && z == voxel.z
	override def foreachUnsafe[U](f: (VoxelCoord) => U): Unit = f(voxel)
	override def foreach[U](f: (VoxelCoord) => U): Unit = f(voxel)

	override def toString(): String = s"SingleVoxelRegion($voxel)"

	override def size: Int = 1
	override def isEmpty = false
}
