package arx.axistential.game.data.world

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/8/15
 * Time: 7:51 AM
 */

import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.general.PhysicalEntityLogic
import arx.core.datastructures.SparseVoxelOctree
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.data.TWorldAuxData
import arx.tmp.game.logic.datastructures.voxelregions.SphereVoxelRegion
import arx.tmp.game.logic.datastructures.voxelregions.VoxelRegion

@SerialVersionUID(1L)
class PhysicalEntityLocationData extends TWorldAuxData {
	val entityLocationOctree : SparseVoxelOctree[TPhysicalEntity] = new SparseVoxelOctree[TPhysicalEntity]()

	def entitiesInRegion (region : VoxelRegion, exact : Boolean) = {
		var center = VoxelCoord.Sentinel
		var radius = 0
		region match {
			case svr : SphereVoxelRegion => {
				center = svr.center
				radius = svr.radius
			}
			case _ => {
				val min = region.min
				val max = region.max
				val dim = max - min
				center = (min + max) / 2
				radius = dim.max
			}
		}

		val baseline = entityLocationOctree.getInVolume(center.x,center.y,center.z,radius)
		// only allow those things that have their feet or center in the region, this may miss some edge cases, admittedly
		// but it gets the main gist
		if (! exact) {
			baseline.filter(ent => region.contains(ent.adjustedFootVoxelPos) || region.contains(ent.position.toVoxelCoord))
		} else {
			baseline.filter(ent => PhysicalEntityLogic.isEntityInRegionInclusive(ent,region) )
		}
	}
}
