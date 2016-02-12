package arx.tmp.game.logic.datastructures.voxelregions

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 9/1/14
 * Time: 8:31 AM
 */

import arx.Prelude._
import arx.core.traits.TSafetyPromotableArxTraversable
import arx.core.units.Dimensions
import arx.core.vec.ReadVec3f
import arx.core.vec.coordinates.TMajorCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.TInfiniteVoxelView
import arx.tmp.game.logic.world.SpatialRegion

trait VoxelRegion extends TSafetyPromotableArxTraversable[VoxelCoord] {
	/** Returns the geometric center of the convex hull of this region */
	def center : VoxelCoord = (min + max) / 2
	/** Returns a new region that contains all voxels in this region and in the <code>other</code> region */
	def intersects (other : VoxelRegion) : Boolean = other.existsUnsafe(v => this.contains(v))
	/** Perform the actual intersection operation. Guaranteed that <code>other</code> will be the larger region */
	protected def performIntersection (other : VoxelRegion) : VoxelRegion
	/** Returns a new region that contains all voxels in this region and in the <code>other</code> region */
	final def intersect (other : VoxelRegion) : VoxelRegion = VoxelRegion.performIntersection(this,other)
	/** Returns a new region that contains all voxels in this region or in the <code>other</code> region */
	def union (other : VoxelRegion) : VoxelRegion = VoxelRegion.apply(Vector(this,other))
	/** Returns a new region that contains all voxels in this region that are not in the <code>other</code> region */
	def without (other : VoxelRegion) : VoxelRegion = {
		val filtered = this.filterNot( other.contains )
		if (filtered.size == this.size) {
			this
		} else {
			VoxelRegion(filtered)
		}
	}
	/** Alias of <code>without</code> */
	def - (other : VoxelRegion) : VoxelRegion = this.without(other)
	/** Whether or not this region contains the given voxel coordinate */
	def contains (v : VoxelCoord): Boolean = contains(v.x,v.y,v.z)
	def contains (x : Int,y : Int,z : Int) : Boolean
	def boundingRegion : VoxelRegion
	/** The dimension, in voxels of the convex hull of this region */
	def boundingDimensions = {
		val br = boundingRegion
		val rawDim = br.max - br.min
		new Dimensions(rawDim.x.voxels,rawDim.y.voxels,rawDim.z.voxels)
	}
	/** The smallest coordinate in all dimensions of a convex hull of this region */
	def min : VoxelCoord
	/** The largest coordinate in all dimensions of a convex hull of this region */
	def max : VoxelCoord
	/** Region representing those voxels that are contained within this region, but adjacent to at least one voxel that is not */
	def edgeVoxels : VoxelRegion
	/** Region representing those voxels that are not contained within this region, but are adjacent to at least one voxel that is */
	def adjacentVoxels : VoxelRegion
	/** This region, represented as an infinite grid of bytes, with 1 bytes for locations inside this region, and 0 bytes for locations outside */
	def asVoxelView : TInfiniteVoxelView[Byte]
	/** The total number of voxels in this region */
	def size : Int

	override def toImmutable(t: VoxelCoord): VoxelCoord = VoxelCoord(t.x,t.y,t.z)

	override def equals (other : Any) = other match {
		case vr : VoxelRegion =>
			this.min == vr.min &&
			this.max == vr.max &&
			this.size == vr.size &&
			this.forallUnsafe( v => vr.contains(v) )
		case _ => false
	}
}


object VoxelRegion {
	/** Change the ordering of the intersection so that the small region is intersecting the large */
	protected def performIntersection(a : VoxelRegion, b : VoxelRegion) : VoxelRegion = {
		if (a.size == 0) {
			b
		} else if (b.size == 0) {
			a
		} else if (a.size > b.size) {
			b.performIntersection(a)
		} else {
			a.performIntersection(b)
		}
	}

	protected def recurseExpand (vr : VoxelRegion) : Vector[VoxelRegion] = vr match {
		case cvr : CompoundVoxelRegion => cvr.regions.flatMap(svr => recurseExpand(svr))
		case other => Vector(other)
	}
	def apply (regions : Vector[VoxelRegion]) : VoxelRegion = {
		val expandedRegions = regions.flatMap(recurseExpand)
		val (irregulars,regulars) = expandedRegions.partition(vr => vr.isInstanceOf[IrregularVoxelRegion])
		val effectiveRegions : Vector[VoxelRegion] = if (irregulars.size <= 1) {
			irregulars ++ regulars
		} else {
			val allIrregularVoxels = irregulars.flatMap { case ivr : IrregularVoxelRegion => ivr.voxels }.distinct
			regulars :+ VoxelRegion(allIrregularVoxels)
		}

		effectiveRegions.size match {
			case 0 => EmptyVoxelRegion
			case 1 => regions.head
			case more => {
				new CompoundVoxelRegion(regions)
			}
		}
	}
	def apply (sr : SpatialRegion) : VoxelRegion = VoxelRegion.apply(VoxelCoord(sr.lowerCorner),VoxelCoord(sr.upperCorner))
	def apply (voxels : Traversable[VoxelCoord]) : VoxelRegion = {
		voxels.size match {
			case 0 => EmptyVoxelRegion
			case 1 => new SingleVoxelRegion(voxels.head)
			case _ => new IrregularVoxelRegion(voxels)
		}
	}
	def apply (voxel : VoxelCoord) : VoxelRegion = new SingleVoxelRegion(voxel)
	def apply (min : VoxelCoord,max : VoxelCoord) : VoxelRegion = fromCorners(min,max)
	def apply (center : TMajorCoord,radius : Int): VoxelRegion = {
		new SphereVoxelRegion(center.toVoxelCoord,radius)
	}
	def fromCorners (min : VoxelCoord,max : VoxelCoord) : VoxelRegion = {
		if ( max == min ) {
			new SingleVoxelRegion(min)
		} else if ( max >= min ) {
			new CubeVoxelRegion(min,max)
		} else {
			EmptyVoxelRegion
		}
	}
	def hollowCube (centeredAt : TMajorCoord, dimensionsOf : ReadVec3f) : VoxelRegion = new CubeSideVoxelRegion(centeredAt,dimensionsOf)
	def hollowCubeFromCorners (min : TMajorCoord, max : TMajorCoord) : VoxelRegion = {
		if (min == max) {
			VoxelRegion(min.toVoxelCoord)
		} else {
			val center = (min.toObjectCoord + max.toObjectCoord) / 2.0f
			val dim = (max.toObjectCoord - min.toObjectCoord)
			hollowCube(center,dim)
		}
	}
}