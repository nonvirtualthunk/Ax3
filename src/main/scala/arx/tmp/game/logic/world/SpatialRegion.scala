package arx.tmp.game.logic.world

import arx.application.Noto
import arx.core.traits.TArxTraversable
import arx.core.vec.ReadVec2i
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.MutableVoxelCoord
import arx.core.vec.coordinates.TMajorCoord
import arx.core.vec.coordinates.VoxelCoord

import scala.collection.GenTraversable
import scala.math._
import scalaxy.loops._

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 9/17/11
 * Time: 1:41 PM
 * Created by nonvirtualthunk
 */

@Deprecated
class SpatialRegion(var center: ReadVec3i,var dimensions: ReadVec3i,var lowerCorner: ReadVec3i,var upperCorner: ReadVec3i) extends TArxTraversable[VoxelCoord] with Serializable
{
	def this (centera: ReadVec3i = Vec3i(0,0,0),dimensionsa: ReadVec3i = Vec3i(0,0,0)) {
		this(centera,dimensionsa,centera - dimensionsa / 2,centera + dimensionsa / 2);
	}


	def foreach[U](f: (VoxelCoord) => U): Unit = {
		for ( x <- lowerCorner.x to upperCorner.x optimized ; y <- lowerCorner.y to upperCorner.y optimized ; z <- lowerCorner.z to upperCorner.z optimized ) {
			f(VoxelCoord(x,y,z))
		}
	}
	def foreachUnsafe[U](f: (VoxelCoord) => U): Unit = {
		val v = MutableVoxelCoord(0,0,0)
		for ( x <- lowerCorner.x to upperCorner.x optimized ; y <- lowerCorner.y to upperCorner.y optimized ; z <- lowerCorner.z to upperCorner.z optimized ) {
			v.x = x
			v.y = y
			v.z = z
			f(v)
		}
	}
	override def size = dimensions.x * dimensions.y * dimensions.z

	def higherCorner: ReadVec3i = upperCorner
	def intersects(other: SpatialRegion): Boolean = {
		abs(other.center.x - this.center.x) < (other.dimensions.x/2 + this.dimensions.x/2) ||
		abs(other.center.y - this.center.y) < (other.dimensions.y/2 + this.dimensions.y/2) ||
		abs(other.center.z - this.center.z) < (other.dimensions.z/2 + this.dimensions.z/2)
	}
	def intersection (other: SpatialRegion): SpatialRegion = {
		val thisLower = this.lowerCorner
		val thisUpper = this.higherCorner
		val otherLower = other.lowerCorner
		val otherUpper = other.higherCorner
		val lower = Vec3i(math.max(thisLower.x,otherLower.x),math.max(thisLower.y,otherLower.y),math.max(thisLower.z,otherLower.z))
		val upper = Vec3i(math.min(thisUpper.x,otherUpper.x),math.min(thisUpper.y,otherUpper.y),math.min(thisUpper.z,otherUpper.z))
		SpatialRegion.fromCorners(lower,upper)
	}

	def split (xsplit : Int , ysplit : Int , zsplit : Int ) : List[SpatialRegion] = {
		val splitV = Vec3i(xsplit,ysplit,zsplit)
		var ret = List[SpatialRegion]()
		val newDims = dimensions / splitV
		for ( x <- 0 until xsplit ; y <- 0 until ysplit ; z <- 0 until zsplit ) {
			val l = Vec3i( lowerCorner + newDims * Vec3i(x,y,z) )
			val u = Vec3i( lowerCorner + newDims * Vec3i(x+1,y+1,z+1) )
			if ( x == xsplit - 1 ) { u.x = upperCorner.x }
			if ( y == ysplit - 1 ) { u.y = upperCorner.y }
			if ( z == zsplit - 1 ) { u.z = upperCorner.z }
			ret ::= SpatialRegion.fromCorners(l,u)
		}
		ret
	}

	def splitIntoGrid (xsplit : Int , ysplit : Int , zsplit : Int ) : Array[Array[Array[SpatialRegion]]] = {
		val splitV = Vec3i(xsplit,ysplit,zsplit)
		val ret = Array.ofDim[SpatialRegion](xsplit,ysplit,zsplit)
		val newDims = dimensions / splitV
		for ( x <- 0 until xsplit ; y <- 0 until ysplit ; z <- 0 until zsplit ) {
			val l = Vec3i( lowerCorner + newDims * Vec3i(x,y,z) )
			val u = Vec3i( lowerCorner + newDims * Vec3i(x+1,y+1,z+1) )
			if ( x == xsplit - 1 ) { u.x = upperCorner.x }
			if ( y == ysplit - 1 ) { u.y = upperCorner.y }
			if ( z == zsplit - 1 ) { u.z = upperCorner.z }
			ret(x)(y)(z) = SpatialRegion.fromCorners(l,u)
		}
		ret
	}


	def contains ( x : Int , y : Int , z : Int ) = {
		x >= lowerCorner.x && x <= upperCorner.x &&
		y >= lowerCorner.y && y <= upperCorner.y &&
		z >= lowerCorner.z && z <= upperCorner.z
	}
	def contains ( point : ReadVec3i ) = {
		point.x >= lowerCorner.x && point.x <= upperCorner.x &&
		point.y >= lowerCorner.y && point.y <= upperCorner.y &&
		point.z >= lowerCorner.z && point.z <= upperCorner.z
	}

	def contains ( point : ReadVec2i ) = {
		point.x >= lowerCorner.x && point.x <= upperCorner.x &&
		point.y >= lowerCorner.y && point.y <= upperCorner.y
	}

	def containsXY(sr: SpatialRegion) = { lte(lowerCorner.xy,sr.lowerCorner.xy) && gte(upperCorner.xy,sr.upperCorner.xy); }

	def lte ( a : ReadVec2i , b : ReadVec2i ) = { a.x <= b.x && a.y <= b.y }
	def gte ( a : ReadVec2i , b : ReadVec2i ) = { a.x >= b.x && a.y >= b.y }
	def lte ( a : ReadVec3i , b : ReadVec3i ) = { a.x <= b.x && a.y <= b.y && a.z <= b.z }
	def gte ( a : ReadVec3i , b : ReadVec3i ) = { a.x >= b.x && a.y >= b.y && a.z >= b.z }
	def contains ( sr : SpatialRegion ) = {
		lte(lowerCorner,sr.lowerCorner) && gte(upperCorner,sr.upperCorner)
	}

	def minX = lowerCorner.x;
	def maxX = upperCorner.x;
	def minY = lowerCorner.y;
	def maxY = upperCorner.y;
	def minZ = lowerCorner.z;
	def maxZ = upperCorner.z;

	def containedPoints = {
		if ( dimensions.x < 100 && dimensions.y < 100 && dimensions.z < 100 ) {
			for ( x <- lowerCorner.x to upperCorner.x ; y <- lowerCorner.y to upperCorner.y ; z <- lowerCorner.z to upperCorner.z ) yield {
				VoxelCoord(x,y,z)
			}
		} else {
			Noto.warn("Asking for contained points for an unacceptably large dimension'd spatial region")
			Nil
		}
	}

	override def equals(p1: Any) = {
		p1 match {
			case sr: SpatialRegion => sr.lowerCorner == this.lowerCorner && sr.upperCorner == this.upperCorner
			case _ => false
		}
	}
}

object SpatialRegion{
	def apply(center: ReadVec3i,dimensions: ReadVec3i): SpatialRegion = { new SpatialRegion(center,dimensions) }
	def fromCorners(lowerCorner: ReadVec3i,upperCorner: ReadVec3i): SpatialRegion = {
		new SpatialRegion((upperCorner + lowerCorner) / 2,upperCorner - lowerCorner + 1,lowerCorner,upperCorner);
	}
	def fromTwoPoints(p1: ReadVec3i,p2: ReadVec3i): SpatialRegion = {
		fromCorners( p1.min(p2) , p1.max(p2) )
	}
	def fromPoints ( l : GenTraversable[VoxelCoord] , buffer : Int = 0 ) : SpatialRegion = {
		val min = Vec3i(Int.MaxValue,Int.MaxValue,Int.MaxValue)
		val max = Vec3i(Int.MinValue,Int.MinValue,Int.MinValue)

		for ( v <- l ) {
			min.x = math.min(min.x,v.x)
			min.y = math.min(min.y,v.y)
			min.z = math.min(min.z,v.z)

			max.x = math.max(max.x,v.x)
			max.y = math.max(max.y,v.y)
			max.z = math.max(max.z,v.z)
		}
		if ( min.x <= max.x && min.y <= max.y && min.z <= max.z ) {
			SpatialRegion.fromTwoPoints(min - buffer,max + buffer)
		} else {
			Noto.warn("Constructing spatial region from no points"); SpatialRegion(Vec3i(0,0,0),Vec3i(0,0,0))
		}
	}
}

class SpatialRegionGroup(var regions: List[SpatialRegion] = List[SpatialRegion]()){
	var boundingRegion: SpatialRegion = updateBoundingRegion()

	def addRegion (region: SpatialRegion) { regions ::= region; boundingRegion = updateBoundingRegion(); }

	def updateBoundingRegion () : SpatialRegion = {
		var maximum = Vec3i(Int.MaxValue,Int.MaxValue,Int.MaxValue)
		var minimum = Vec3i(Int.MinValue,Int.MinValue,Int.MinValue)

		for ( region <- regions ){
			val regionMin = region.lowerCorner
			val regionMax = region.higherCorner
			maximum = Vec3i( max(maximum.x,regionMax.x),max(maximum.y,regionMax.y),max(maximum.z,regionMax.z) )
			minimum = Vec3i( min(minimum.x,regionMin.x),min(minimum.y,regionMin.y),min(minimum.z,regionMin.z) )
		}

		new SpatialRegion( (maximum + minimum) / 2 , maximum - minimum )
	}

	def intersects (other: SpatialRegionGroup): Boolean = {
		boundingRegion.intersects(other.boundingRegion) && regions.exists( r1 => other.regions.exists( r2 => r1.intersects(r2) ) )
	}
	def intersects (other: SpatialRegion): Boolean = {
		boundingRegion.intersects(other) && regions.exists( other.intersects(_) )
	}
}




class WorldRegion (val min : VoxelCoord,val max : VoxelCoord) {
	def containedVoxels = {
		var ret = List[VoxelCoord]()
		var x = min.x;while ( x <= max.x ) {
			var y = min.y;while ( y <= max.y ) {
				var z = min.z;while ( z <= max.z ) {
					ret ::= VoxelCoord(x,y,z)
				z += 1}
			y += 1}
		x += 1}
		ret
	}

	def center = VoxelCoord((min.x + max.x) >> 1,(min.y + max.y) >> 1,(min.z + max.z) >> 1)
	def dimensions = max - min
	def contains ( t : TMajorCoord ) = {
		val v = t.toVoxelCoord
		v.x >= min.x && v.x <= max.x && v.y >= min.y && v.y <= max.y && v.z >= min.z && v.z <= max.z
	}

	override def toString = "World Region(" + min + ", " + max + ")"
}
object WorldRegion {
	def apply ( m : VoxelCoord , x : VoxelCoord ) = new WorldRegion(
	VoxelCoord(m.min(x)),VoxelCoord(m.max(x)))
}