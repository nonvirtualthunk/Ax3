package arx.axistential.game.logic.general

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/31/13
 * Time: 3:25 PM
 */

import arx.Prelude._
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.physics.CollisionShape
import arx.axistential.game.logic.physics.CubeCollisionShape
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.world.World

abstract class Location {
	def centerPoint : ObjectCoord
	def shape : CollisionShape
	def world : World
}

object Location {
	case class VoxelLocation ( point : VoxelCoord, inWorld : World ) extends Location {
		val centerPoint = point.toObjectCoord
		val shape = new CubeCollisionShape(oneVoxelCube)
		shape.frameOfReferenceChanged(PointFrameOfReference(centerPoint))
		def world = inWorld
	}
	implicit class EntityLocation ( val entity : TPhysicalEntity ) extends Location {
		def centerPoint = entity.position
		def shape: CollisionShape = entity.collisionShape
		def world = entity.world
	}
}