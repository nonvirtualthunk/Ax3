package arx.tmp.game.logic.datastructures.voxelregions

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/23/15
 * Time: 1:20 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.datastructures.OrderedSphericalIterator
import arx.core.vec.coordinates.MutableVoxelCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.FunctionVoxelView
import arx.tmp.game.logic.datastructures.TInfiniteVoxelView

class SphereVoxelRegion protected[datastructures] (override val center : VoxelCoord, val radius : Int) extends VoxelRegion {
	val r2 = radius * radius
	override def performIntersection(other: VoxelRegion): VoxelRegion = new IrregularVoxelRegion(this.filter(other.contains))
	override def boundingRegion: VoxelRegion = VoxelRegion(center - radius,center + radius)
	override def max: VoxelCoord = {Noto.severeError("Asking for the max of a sphere voxel region doesn't make sense"); center + radius}
	override def asVoxelView: TInfiniteVoxelView[Byte] = new FunctionVoxelView[Byte]((x,y,z) => this.contains(x,y,z) ? 1.toByte | 0.toByte)
	override def min: VoxelCoord = {Noto.warn("Asking for the min of a sphere voxel region doesn't make sense"); center - radius}
	override def edgeVoxels: VoxelRegion = if (radius == 1) { this } else { new NegatingVoxelRegion(this,new SphereVoxelRegion(center,radius - 1)) }
	override def contains(x: Int, y: Int, z: Int): Boolean = {
		val dx = x - center.x
		val dy = y - center.y
		val dz = z - center.z
		dx*dx+dy*dy+dz*dz <= r2
	}
	override def foreachUnsafe[U](f: (VoxelCoord) => U): Unit = {
		val tmp = MutableVoxelCoord(0,0,0)
		val iter = new OrderedSphericalIterator(radius)
		while (iter.hasNext) {
			val offset = iter.next ()
			tmp.x = center.x + offset.x
			tmp.y = center.y + offset.y
			tmp.z = center.z + offset.z
			f(tmp)
		}
	}
	override def foreach[U](f: (VoxelCoord) => U): Unit = {
		val iter = new OrderedSphericalIterator(radius)
		while (iter.hasNext) {
			val offset = iter.next ()
			f(center + offset)
		}
	}


	override def toString(): String = s"SphereVoxelRegion(center: $center, radius: $radius)"

	def canEqual(other: Any): Boolean = other.isInstanceOf[SphereVoxelRegion]
	override def equals(other: Any): Boolean = other match {
		case that: SphereVoxelRegion =>
			(that canEqual this) &&
			center == that.center &&
			radius == that.radius
		case _ => false
	}
	override def hashCode(): Int = {
		val state = Seq(super.hashCode(), center, radius)
		state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
	}

	override def isEmpty = radius < 0

	// an actual sphere is easy, but it's not clear precisely how to round it to get this one right
//	override def size: Int = super.size
	/** Region representing those voxels that are not contained within this region, but are adjacent to at least one voxel that is */
	override def adjacentVoxels: VoxelRegion = VoxelRegion(center,radius+1)
}
