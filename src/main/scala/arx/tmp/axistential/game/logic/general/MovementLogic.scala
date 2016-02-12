package arx.axistential.game.logic.general

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 4/12/15
 * Time: 10:00 AM
 */

import arx.axistential.game.data.world.DesignationData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.entities.CreatureEntity
import arx.axistential.game.entities.groups.Colony
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.voxelregions.VoxelRegion

object MovementLogic {
	def moveCostFunction (entity : TPhysicalEntity) = {
		entity match {
			// Members of the player's colony should avoid areas designated for construction, everyone else really
			// has no reason to care about that
			case ce : CreatureEntity if ce.aiGroup.isInstanceOf[Colony] =>
				val DD = entity.world.aux[DesignationData]

				(v1 : VoxelCoord, v2 : VoxelCoord,climb : Int,jump : Int) => {
					val desig = DD.designationAt(v2.x,v2.y,v2.z)
					val desigMult = if (desig == DD.additionByte) { 5.0f } else if (desig == DD.immediateAdditionByte) { 10.0f } else { 1.0f }
					v1.distanceInVoxelsTo(v2) * desigMult
				}
			case _ =>
				(v1 : VoxelCoord, v2 : VoxelCoord,climb : Int,jump : Int) => {
					v1.distanceInVoxelsTo(v2)
				}
		}
	}
	
	def obstructionFunction (world : World) = {
		val TD = world.aux[TerrainData]
		(v : VoxelCoord) => TD.isSolid(v)
	}

	def isSupportedFunction (world : World) = {
		val TD = world.aux[TerrainData]
		(v : VoxelCoord) => TD.isSolid(v.x,v.y,v.z - 1)
	}

	def targetRegionFor (mover : TPhysicalEntity, target : TPhysicalEntity) = {
//		val targetHalfDim = target.boundingDimensions.inVoxels * 0.5f
//		val min = (target.position - targetHalfDim).toVoxelCoord
//		val max = (target.position + targetHalfDim).toVoxelCoord
//		VoxelRegion(min,max)
		VoxelRegion.hollowCube(target.position,target.boundingDimensions.inVoxels + 1.0f)
	}

	def targetRegionFor (mover : TPhysicalEntity, targetVoxel : VoxelCoord) = {
		val H = mover.boundingDimensions.z.inVoxels.toInt

		val min = targetVoxel - Vec3i(1,1,H + 1)
		val max = targetVoxel + Vec3i(1,1,1)

		VoxelRegion.fromCorners(min,max).without(VoxelRegion(targetVoxel))
	}
}
