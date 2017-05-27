package arx.rog2.game.engine

/**
  * TODO: Add javadoc
  */

import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.game.GameEngine
import arx.engine.game.components.GameComponent
import arx.rog2.engine.RogComponent
import arx.rog2.game.data.entity.Physical
import arx.rog2.game.data.world.EntityOcclusionData
import arx.rog2.game.data.world.Terrain

class RogPhysicsGameComponent(engine: GameEngine) extends GameComponent(engine) with RogComponent {
	override protected def update(dt: UnitOfTime): Unit = {
		val TD = world[Terrain]
		for (ent <- entitiesWithAuxData[Physical]) {
			if (TD.voxel(ent.position.minusZ(1)).isSentinel) {
				ent.position = ent.position.minusZ(1)
			}
		}
	}


	def entitiesAtLocation(v: VoxelCoord) = {
		world[EntityOcclusionData].entities.get(v).filter(e => {
			val PD = e[Physical]
			val dimi = PD.dimensions.inVoxels.ceili
			PD.heldIn.isEmpty &&
				PD.position.x <= v.x && PD.position.y <= v.y && PD.position.z <= v.z &&
				PD.position.x + dimi.x > v.x && PD.position.y + dimi.y > v.y && PD.position.z + dimi.z > v.z
		})
	}
}
