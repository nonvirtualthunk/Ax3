package arx.tmp.game.logic.datastructures.voxelregions

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/23/15
 * Time: 1:19 PM
 */

import arx.core.traits.TArxTraversable
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.HashVoxelByteStore
import arx.tmp.game.logic.datastructures.TInfiniteVoxelView

import scalaxy.loops._
import scalaxy.streams._

class IrregularVoxelRegion protected[datastructures] (val voxels : TArxTraversable[VoxelCoord]) extends VoxelRegion {
	lazy val voxelView = {
		val bs = new HashVoxelByteStore()
		voxels.foreachUnsafe(v => bs(v) = 1.toByte)
		bs
	}
	lazy val edgeVoxels = VoxelRegion(voxels.filter(v => optimize {
		(0 until 6).exists(q => voxelView(v.x + cardinalsX(q),v.y + cardinalsY(q),v.z + cardinalsZ(q)) == 0)
	}))
	lazy val adjacentVoxels = {
		var adj = Vector[VoxelCoord]()
		voxels.foreachUnsafe { v =>
			for (q <- 0 until 6 optimized) {
				if (! this.contains(v.x + cardinalsX(q),v.y + cardinalsY(q),v.z + cardinalsZ(q))) {
					adj :+= VoxelCoord(v)
				}
			}
		}
		VoxelRegion(adj)
	}
	lazy val min = { var m = voxels.head; voxels.foreachUnsafe(v => m = m.min(v)); m }
	lazy val max = { var m = voxels.head; voxels.foreachUnsafe(v => m = m.max(v)); m }
	override def performIntersection(other: VoxelRegion): VoxelRegion = {
		VoxelRegion(voxels.filter(other.contains))
	}
	override def without (other : VoxelRegion) : VoxelRegion = {
		VoxelRegion(voxels.filterNot(other.contains))
	}
	lazy val boundingRegion: VoxelRegion = VoxelRegion(min,max)
	override def asVoxelView: TInfiniteVoxelView[Byte] = voxelView
	override def contains(x: Int, y: Int, z: Int): Boolean = voxelView(x,y,z) != 0
	override def foreachUnsafe[U](f: (VoxelCoord) => U): Unit = voxels.foreachUnsafe(f)
	override def foreach[U](f: (VoxelCoord) => U): Unit = voxels.foreach(f)
	override def hashCode = voxels.hashCode()
	override def equals (other : Any) = other match {
		case ivr : IrregularVoxelRegion => ivr.voxels == this.voxels
		case _ => super.equals(other)
	}
	override def isEmpty = voxels.isEmpty
}
