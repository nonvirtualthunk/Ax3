package arx.eldr.game.entity.data

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.units.Dimensions
import arx.core.units.UnitOfVolume
import arx.core.vec.ReadVec3f
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec3f
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.VoxelCoord
import arx.eldr.game.archetypes.Material
import arx.engine.data.TGameEntityAuxData
import arx.engine.entity.GameEntity
import arx.engine.entity.TGameEntity

import scalaxy.loops._

class PhysicalData extends TGameEntityAuxData {
	protected var _position : VoxelCoord = VoxelCoord.Sentinel
	var shape : PhysicalShape = new CubeShape(Vec3i.One)
	var madeOf : List[Material] = Nil
	var heldBy : Option[TGameEntity] = None
	/** Indicates that this entity cannot be moved without its destruction */
	var staticObject : Boolean = true
	/** Indicates that this entity fully occupies its space and cannot be moved through normally */
	var occupiesSpace : Boolean = true

	def position : VoxelCoord = heldBy match {
		case Some(holder) => holder[PhysicalData].position
		case None => _position
	}
	def position_= (v : VoxelCoord) { _position = v }
	def bottomLeftPosition = position
	def occupiedRegion = shape.occupiedRegion(bottomLeftPosition)

	def dimensions = shape.boundingDimensions
}

trait PhysicalShape {
	def occupiedRegion(relativeTo:VoxelCoord) : VoxelRegion
	def boundingDimensions : ReadVec3i
	def exactDimensions : Dimensions = {
		val bdim = boundingDimensions
		new Dimensions(bdim.x.voxels, bdim.y.voxels, bdim.z.voxels)
	}
	def volume : UnitOfVolume = exactDimensions.volume
}

case class CubeShape(dimensions : ReadVec3i) extends PhysicalShape {
	override def occupiedRegion(relativeTo: VoxelCoord): VoxelRegion = VoxelRegion(relativeTo, relativeTo + (dimensions - 1))
	override def boundingDimensions: ReadVec3i = dimensions
}

class SubVoxelShape(val dimensions : ReadVec3f) extends PhysicalShape {
	override def occupiedRegion(relativeTo: VoxelCoord): VoxelRegion = VoxelRegion(relativeTo, relativeTo + (dimensions.ceili - 1))
	override def boundingDimensions: ReadVec3i = dimensions.ceili
	override def exactDimensions: Dimensions = new Dimensions(dimensions.x.voxels, dimensions.y.voxels, dimensions.z.voxels)
}

object SubVoxelShape {
	def apply(dim : ReadVec3f) = new SubVoxelShape(dim)
	def apply(dim : Dimensions) = new SubVoxelShape(dim.inVoxels)
}