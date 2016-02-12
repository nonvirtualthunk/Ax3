package arx.tmp.game.logic.datastructures.voxelregions

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/23/15
 * Time: 1:19 PM
 */

import arx.Prelude._
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.MergedInfiniteVoxelView
import arx.tmp.game.logic.datastructures.TInfiniteVoxelView

import scalaxy.streams._

class CompoundVoxelRegion protected[datastructures] (val regions : Vector[VoxelRegion]) extends VoxelRegion {
	posit(regions.size > 1,"CompoundVoxelRegion with only 1 sub-region")
	lazy val min = regions.map(_.min).reduceLeft(_.min(_))
	lazy val max = regions.map(_.max).reduceLeft(_.max(_))
	lazy val boundingRegion = VoxelRegion(min,max)


	override def intersects(other: VoxelRegion): Boolean = regions.exists( r => r.intersects(other) )
	override def performIntersection(other: VoxelRegion): VoxelRegion = {
		VoxelRegion(regions.map(_.intersect(other)).filter(_.nonEmpty))
	}
	override def without (other : VoxelRegion) : VoxelRegion = {
		VoxelRegion(regions.map(_.without(other)).filter(_.nonEmpty))
	}
	lazy val edgeVoxels = {
		val vv = this.asVoxelView
		val baseEdgeRegions = regions.flatMap( _.edgeVoxels ).distinct
		VoxelRegion(baseEdgeRegions.filter(v => optimize {
			(0 until 6).exists(q => vv(v.x + cardinalsX(q),v.y + cardinalsY(q),v.z + cardinalsZ(q)) == 0)
		}))
	}
	lazy val adjacentVoxels = {
		// start with all the adjacents of all sub-regions
		val allPossibleAdjacents = regions.map(_.adjacentVoxels).reduceLeft(_ union _)
		// and remove any candidate adjacent that is contained by any sub-region
		regions.foldLeft(allPossibleAdjacents) {
			case (remainingRegion,constituentRegion) => remainingRegion without constituentRegion
		}
	}
	lazy val asVoxelView: TInfiniteVoxelView[Byte] = new MergedInfiniteVoxelView[Byte](regions.map(_.asVoxelView).toArray,0.toByte)
	override def contains(x: Int, y: Int, z: Int): Boolean = regions.exists(r => r.contains(x,y,z))
	override def foreachUnsafe[U](f: (VoxelCoord) => U): Unit = regions.foreach(r => r.foreachUnsafe(f))
	override def foreach[U](f: (VoxelCoord) => U): Unit = regions.foreach(r => r.foreach(f))
	override lazy val size: Int = regions.isum(_.size)

	override def hashCode = regions.hashCode()
	override def equals (other: Any) = other match {
		case cvr : CompoundVoxelRegion => cvr.regions == this.regions
		case o => super.equals(o)
	}

	override def toString(): String =  s"CompoundVoxelRegion(${regions.map(_.toString())})"
	override def isEmpty = regions.forall(r => r.isEmpty)
}
