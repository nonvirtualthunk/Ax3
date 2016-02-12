package arx.axistential.game.logic.physics

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/13/13
 * Time: 2:03 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.core.mat.TFrameOfReference
import arx.core.traits.TSentinel
import arx.core.traits.TSentinelable
import arx.core.units.Dimensions
import arx.core.units.UnitOfVolume
import arx.core.vec.coordinates.MutableObjectCoord
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.ReadVec3f
import arx.core.vec.Vec3i
import arx.tmp.game.logic.datastructures.ByteTalea
import arx.tmp.game.logic.datastructures.GenericTaleaGrid

import scala.collection.mutable.ListBuffer
import scalaxy.loops._

abstract class CollisionShape extends TSentinelable with Serializable {
	def frameOfReferenceChanged ( frameOfReference : TFrameOfReference )
	def allContainedVoxels : Traversable[VoxelCoord]
	def allUnderVoxels : Traversable[VoxelCoord]
	def boundingDimensions : Dimensions
	def volume : UnitOfVolume

	/**
	 * t value at which intersection occurs, or -1 if no intersection.
	 * This function assumes that the base state of this collision shape is _not_ intersecting
	 * anything, and will not detect such a case.
 *
	 * @param delta
	 * @param axis
	 * @param grid
	 * @return
	 */
	def intersect ( delta : Float, axis : Int, grid : GenericTaleaGrid[Byte,ByteTalea] ) : Float

	def intersect ( grid : GenericTaleaGrid[Byte,ByteTalea] ) : Option[ObjectCoord]

	def scaledBy (scale : ReadVec3f) : CollisionShape
}

class CubeCollisionShape(dims:Dimensions) extends CollisionShape {
	val voxelDims = dims.inVoxels
	var intDims = Vec3i(voxelDims.round).max( Vec3i.One )
	val allVoxels = new ListBuffer[VoxelCoord]
	val underVoxels = new ListBuffer[VoxelCoord]
	var centerPoint : ObjectCoord = ObjectCoord.Sentinel
	var minExtent : ObjectCoord = MutableObjectCoord(0,0,0)
	var maxExtent : ObjectCoord = MutableObjectCoord(0,0,0)
	
	def frameOfReferenceChanged(frameOfReference: TFrameOfReference): Unit = {
//		if ( centerPoint.isSentinel

		centerPoint = frameOfReference.transformToFrameOfReference(ObjectCoord.Zero)
		val offset = centerPoint.toVoxelCoord - (intDims/2)
		val underOffset = frameOfReference.transformToFrameOfReference(ObjectCoord(0.0f,0.0f,-0.1f)).toVoxelCoord - (intDims/2)

		allVoxels.clear()
		underVoxels.clear()
		var x = 0; while ( x < intDims.x  ) {
			var y = 0; while ( y < intDims.y ) {
				underVoxels.append( underOffset.plusXYZ(x,y,0) )
				var z = 0; while ( z < intDims.z ) {
					allVoxels.append( offset.plusXYZ(x,y,z) )
				z += 1}
			y += 1}
		x += 1}

		minExtent = centerPoint - voxelDims * 0.5f
		maxExtent = centerPoint + voxelDims * 0.5f
	}
	
	def allContainedVoxels : Traversable[VoxelCoord] = {
		allVoxels
	}


	def allUnderVoxels: Traversable[VoxelCoord] = underVoxels

	def boundingDimensions : Dimensions = {
		dims
	}

	def volume = dims.volume

	def intersect(delta: Float, axis : Int, grid: GenericTaleaGrid[Byte, ByteTalea]): Float = {
		val arr = Array.ofDim[Int](3)
		if ( delta == 0.0f ) { return -1.0f }
		val orthoAxisA = (axis+1)%3
		val orthoAxisB = (axis+2)%3

		val orthoAStart = ObjectCoord.toVoxelCoordX( minExtent(orthoAxisA) )
		val orthoAEnd = ObjectCoord.toVoxelCoordX( maxExtent(orthoAxisA) )

		val orthoBStart = ObjectCoord.toVoxelCoordX( minExtent(orthoAxisB) )
		val orthoBEnd = ObjectCoord.toVoxelCoordX( maxExtent(orthoAxisB) )

		var orthoA = orthoAStart; while ( orthoA <= orthoAEnd ) {
			arr(orthoAxisA) = orthoA
			var orthoB = orthoBStart; while ( orthoB <= orthoBEnd ) {
				arr(orthoAxisB) = orthoB
				if ( delta < 0.0f ) {
					arr(axis) = ObjectCoord.toVoxelCoordX(minExtent(axis) + delta)
				} else if ( delta > 0.0f ) {
					arr(axis) = ObjectCoord.toVoxelCoordX(maxExtent(axis) + delta)
				}

				if ( grid(arr(0),arr(1),arr(2)) > 0 ) {
					if ( delta < 0.0f ) {
						return absf((minExtent(axis) - floorf(minExtent(axis))) / delta)
					} else {
						return absf((ceilf(maxExtent(axis)) - maxExtent(axis)) / delta)
					}
				}

			orthoB += 1}
		orthoA += 1}
		-1.0f
	}

	def intersect(grid: GenericTaleaGrid[Byte, ByteTalea]): Option[ObjectCoord] = {
		val min = minExtent.toVoxelCoord
		val max = maxExtent.toVoxelCoord

		val window = grid.windowCenteredOnTaleaAt(min,readOnly = true)
		for ( x <- min.x to max.x optimized ; y <- min.y to max.y optimized ; z <- min.z to max.z optimized ) {
			if ( window(x - window.center.x,y - window.center.y,z - window.center.z) > 0 ) { return Some(VoxelCoord(x,y,z).toObjectCoord) }
		}
		None
	}

	override def scaledBy(scale: ReadVec3f): CollisionShape = {
		new CubeCollisionShape(dims * scale)
	}
}


object CollisionShape {
	val Sentinel : CollisionShape = new CollisionShape with TSentinel {
		def frameOfReferenceChanged(frameOfReference: TFrameOfReference): Unit = {}

		def allContainedVoxels: Traversable[VoxelCoord] = Nil


		def allUnderVoxels: Traversable[VoxelCoord] = Nil

		def boundingDimensions: Dimensions = Dimensions.Zero

		def volume: UnitOfVolume = zeroMeters3

		def intersect(delta: Float, axis : Int, grid: GenericTaleaGrid[Byte, ByteTalea]): Float = -1f

		def intersect(grid: GenericTaleaGrid[Byte, ByteTalea])= None

		protected def readResolve : Object = CollisionShape.Sentinel

		override def scaledBy(scale: ReadVec3f): CollisionShape = this
	}
}
