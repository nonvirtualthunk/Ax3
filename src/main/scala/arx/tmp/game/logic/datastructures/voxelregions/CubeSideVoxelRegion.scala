package arx.tmp.game.logic.datastructures.voxelregions

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/23/15
 * Time: 1:16 PM
 */

import arx.core.vec.ReadVec3f
import arx.core.vec.coordinates.MutableVoxelCoord
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.TMajorCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.FunctionVoxelView
import arx.tmp.game.logic.datastructures.TInfiniteVoxelView

import scala.collection.mutable
import scalaxy.loops._

// Could be easily extended to be a hull of arbitrary thickness, not just 1 thick edge
class CubeSideVoxelRegion(centeredAt : TMajorCoord,dimensions : ReadVec3f) extends VoxelRegion {
	protected val halfDims = dimensions / 2.0f
	val min = (centeredAt.toObjectCoord - halfDims).toVoxelCoord
	val max = (centeredAt.toObjectCoord + halfDims).toVoxelCoord

	override def performIntersection(other: VoxelRegion): VoxelRegion = VoxelRegion(this.filter(other.contains))
	override def boundingRegion: VoxelRegion = this
	override def asVoxelView: TInfiniteVoxelView[Byte] = new FunctionVoxelView[Byte]((x,y,z) => if (contains(x,y,z)) { 1.toByte } else { 0 })
	override def edgeVoxels: VoxelRegion = this
	override def contains(x: Int, y: Int, z: Int): Boolean = {
		if (x >= min.x && x <= max.x && y >= min.y && y <= max.y && z >= min.z && z <= max.z) {
			// if at least one of the coordinates is on the edge, then we are on the face of the cube
			x == min.x || x == max.x || y == min.y || y == max.y || z == min.z || z == max.z
		} else {
			false
		}
	}

	/** Region representing those voxels that are not contained within this region, but are adjacent to at least one voxel that is */
	override def adjacentVoxels: VoxelRegion = VoxelRegion.hollowCube(centeredAt,dimensions + 2)

	override def foreachUnsafe[U](f: (VoxelCoord) => U): Unit = {
		val tmp = new MutableVoxelCoord(0,0,0)
		val iter = new CubeSideIterator(min,max)
		iter.foreach( (x,y,z) => {
			tmp.x = x; tmp.y = y; tmp.z = z;
			f(tmp)
		})
	}
	override def foreach[U](f: (VoxelCoord) => U): Unit = {
		val iter = new CubeSideIterator(min,max)
		iter.foreach( (x,y,z) => f(VoxelCoord(x,y,z)))
	}

	override def toString() = s"CubeSideVoxelRegion($min to $max)"
}

class CubeSideIterator (min : VoxelCoord,max : VoxelCoord) {
	def this (center : ObjectCoord, baseDims : ReadVec3f ){
		this((center - baseDims / 2.0f).toVoxelCoord,(center + baseDims / 2.0f).toVoxelCoord)
	}


	def foreach[T] ( f : (Int,Int,Int) => T ) {
		val hit = new mutable.HashSet[VoxelCoord]()
		val arr = Array(0,0,0)
		for ( axis <- 0 until 3 optimized ) {
			val orthoAxisA = (axis+1)%3
			val orthoAxisB = (axis+2)%3

			val offA = if (axis >= 2) { 1 } else { 0 }
			val offB = if (axis >= 1) { 1 } else { 0 }
			for ( orthoA <- min( orthoAxisA ) + offA to max( orthoAxisA ) - offA optimized ) {
				for ( orthoB <- min( orthoAxisB ) + offB to max( orthoAxisB ) - offB optimized ) {
					arr(axis) = min(axis)
					arr(orthoAxisA) = orthoA
					arr(orthoAxisB) = orthoB
					f(arr(0),arr(1),arr(2))

					arr(axis) = max(axis)
					f(arr(0),arr(1),arr(2))
				}
			}
		}
	}
}