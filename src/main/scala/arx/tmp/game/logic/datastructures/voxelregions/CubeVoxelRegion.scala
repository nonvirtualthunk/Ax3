package arx.tmp.game.logic.datastructures.voxelregions

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/23/15
 * Time: 1:19 PM
 */

import arx.core.vec.coordinates.MutableVoxelCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.FunctionVoxelView

import scalaxy.loops._
import scalaxy.streams._

class CubeVoxelRegion protected[datastructures]  (val min : VoxelCoord,val max : VoxelCoord) extends VoxelRegion {
	override val center = (min + max) / 2
	val dimensions = max - min + 1
	val halfDimensions = dimensions / 2
	override def intersects(other: VoxelRegion): Boolean = other match {
		case cvr : CubeVoxelRegion => optimize {
			(0 until 3).forall(a => min(a) <= other.max(a) || max(a) >= other.min(a))
		}
		case _ => super.intersects(other)
	}
	override def without (other : VoxelRegion) : VoxelRegion = other match {
		case svr : SingleVoxelRegion => new NegatingVoxelRegion(this,svr)
		case cvr : CubeVoxelRegion => {
			VoxelRegion(
				Vector(
					VoxelRegion(VoxelCoord(this.min.x,this.min.y,cvr.max.z+1),VoxelCoord(this.max.x,this.max.y,this.max.z)),
					VoxelRegion(VoxelCoord(this.min.x,this.min.y,this.min.z),VoxelCoord(this.max.x,this.max.y,cvr.min.z-1)),
					VoxelRegion(VoxelCoord(this.min.x,cvr.max.y+1,cvr.min.z),VoxelCoord(this.max.x,this.max.y,cvr.max.z)),
					VoxelRegion(VoxelCoord(this.min.x,this.min.y,cvr.min.z),VoxelCoord(this.max.x,cvr.min.y-1,cvr.max.z)),
					VoxelRegion(VoxelCoord(cvr.max.x+1,cvr.min.y,cvr.min.z),VoxelCoord(this.max.x,cvr.max.y,cvr.max.z)),
					VoxelRegion(VoxelCoord(this.min.x,cvr.min.y,cvr.min.z),VoxelCoord(cvr.min.x-1,cvr.max.y,cvr.max.z))
				)
			)
		}
		case ivr : IrregularVoxelRegion => {
			super.without(other)
		}
	}

	lazy val _edgeVoxels: VoxelRegion = {
		val oCenter = (min.toObjectCoord + max.toObjectCoord) * 0.5f
		VoxelRegion.hollowCube(oCenter,(max - min))
//		var voxels = Set[VoxelCoord]()
//		for (axis <- 0 until 3 optimized; minimax <- 0 to 1 optimized) {
//			val start = min.plusAxis(axis,minimax * (dimensions(axis)-1))
//			val orthoAxis = (axis + 1) % 3
//			val upAxis = (axis + 2) % 3
//			val ortho = VoxelCoord(0,0,0).plusAxis(orthoAxis,1)
//			val up = VoxelCoord(0,0,0).plusAxis(upAxis,1)
//			for ( dO <- 0 until dimensions(orthoAxis) optimized ; dU <- 0 until dimensions(upAxis) optimized ) {
//				voxels += VoxelCoord(start.x + ortho.x * dO + up.x * dU,start.y + ortho.y * dO + up.y * dU,start.z + ortho.z * dO + up.z * dU)
//			}
//		}
//		VoxelRegion(voxels)
	}

	/** Region representing those voxels that are not contained within this region, but are adjacent to at least one voxel that is */
	override def adjacentVoxels: VoxelRegion = {
		val oCenter = (min.toObjectCoord + max.toObjectCoord) * 0.5f
		VoxelRegion.hollowCube(oCenter,(max - min) + 2)
	}

	override def edgeVoxels: VoxelRegion = _edgeVoxels

	override def boundingRegion: VoxelRegion = this
	val asVoxelView = new FunctionVoxelView[Byte]((x,y,z) => if (contains(x,y,z)) { 1.toByte } else { 0 })
	override def contains(x: Int, y: Int, z: Int): Boolean = x >= min.x && y >= min.y && z >= min.z && x <= max.x && y <= max.y && z <= max.z
	override def performIntersection(other: VoxelRegion): VoxelRegion = other match {
		case cvr : CubeVoxelRegion => VoxelRegion( cvr.min.max(this.min) , cvr.max.min(this.max) )
		case pvr : CompoundVoxelRegion => pvr.performIntersection(this) // we have faster inclusion testing than a compound, so switch this around
		case hvr : CubeSideVoxelRegion if hvr.min >= this.min && hvr.max <= this.max => hvr // if it's a cube-side that's within our bounds, just return the cube-side
		case _ => VoxelRegion( this.filter(other.contains) )
	}
	override def foreachUnsafe[U](f: (VoxelCoord) => U): Unit = {
		val v = MutableVoxelCoord(0,0,0)
		for (x<-min.x to max.x optimized;y<-min.y to max.y optimized;z <- min.z to max.z optimized) {
			v.x = x
			v.y = y
			v.z = z
			f(v)
		}
	}
	override def foreach[U](f: (VoxelCoord) => U): Unit = {
		for (x<-min.x to max.x optimized;y<-min.y to max.y optimized;z <- min.z to max.z optimized) {
			f(VoxelCoord(x,y,z))
		}
	}
	override def size = dimensions.x * dimensions.y * dimensions.z
	override def isEmpty = size == 0

	override def hashCode = min.hashCode * 31 + max.hashCode
	override def equals (other : Any) = other match {
		case cvr : CubeVoxelRegion => cvr.min == this.min && cvr.max == this.max
		case _ => super.equals(other)
	}

	override def toString(): String = s"CubeVoxelRegion($min to $max)"
}
