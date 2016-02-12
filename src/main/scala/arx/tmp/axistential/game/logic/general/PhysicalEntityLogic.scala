package arx.axistential.game.logic.general

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/8/15
 * Time: 8:08 AM
 */

import arx.Prelude._
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.core.vec.Vec3f
import arx.tmp.game.logic.datastructures.voxelregions.VoxelRegion

import scalaxy.loops._

object PhysicalEntityLogic {
	def isEntityInRegionInclusive(ent: TPhysicalEntity, region: VoxelRegion): Boolean = {
		if (region.contains(ent.adjustedFootVoxelPos) || region.contains(ent.position.toVoxelCoord)) {
			true
		} else {
			val min = (ent.position - ent.boundingDimensions.inVoxels).toVoxelCoord
			val max = (ent.position + ent.boundingDimensions.inVoxels).toVoxelCoord
			for (x <- min.x to max.x optimized ; y <- min.y to max.y optimized ; z <- min.z to max.z optimized ) {
				if (region.contains(x,y,z)) { return true }
			}
			false
		}
	}

	def minimumDistanceBetweenEntities (a : TPhysicalEntity, b : TPhysicalEntity) = {
		val distByAxis = Vec3f(0.0f,0.0f,0.0f)
		for (axis <- 0 until 3 optimized) {
			val combinedDim = a.boundingDimensions(axis).inVoxels + b.boundingDimensions(axis).inVoxels
			distByAxis(axis) = ((a.position(axis) - b.position(axis)).abs - combinedDim).max(0.0f)
		}
		distByAxis.lengthSafe.voxels
	}
}
