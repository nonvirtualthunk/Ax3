package arx.axistential.game.entities.helpers

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/3/15
 * Time: 8:16 AM
 */

import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.physics.CubeCollisionShape
import arx.tmp.game.logic.datastructures.voxelregions.VoxelRegion

class Zone (val region : VoxelRegion) extends TPhysicalEntity {
	this.dynamic = false
	this.ghost = true

	this.collisionShape = new CubeCollisionShape(region.boundingDimensions)
}
