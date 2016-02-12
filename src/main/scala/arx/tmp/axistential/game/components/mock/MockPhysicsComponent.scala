package arx.axistential.game.components.mock

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 9/25/14
 * Time: 9:03 AM
 */

import arx.Prelude._
import arx.axistential.game.data.world.PhysicalEntityLocationData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.core.query.ContinuousQueryListener
import arx.core.units.NoSpeed
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.ObjectCoord

class MockPhysicsComponent extends GameEngineComponent with ContinuousQueryListener[TPhysicalEntity]{
	lazy val physicalEntities = world.createEntityTypeQuery[TPhysicalEntity].withListener(this,fireOnExistingResults = true)
	lazy val PELD = world.aux[PhysicalEntityLocationData]
	lazy val TD = world.aux[TerrainData]


	override def initialize(): Unit = {
		// trigger lazy load
		physicalEntities.results
		super.initialize ()
	}

	override def update(time: UnitOfTime): Unit = {

		for ( ent <- physicalEntities ; if ent.dynamic && ! ent.ghost) {
			if ( ent.motive ) {
				ent.velocity = ent.motiveVector.copy()
				if ((ent.velocity.z eq NoSpeed) && ent.adjustedFootPos.z > 0.005f) {
					ent.velocity.z = -0.05.m_s
				}

				if ( ! ent.motiveVector.isEmpty ) {
					ent.motiveVector.x = zero_ms//NoVelocity
					ent.motiveVector.y = zero_ms
					ent.motiveVector.z = NoSpeed
				}

				while (TD.isSolid(ent.adjustedFootVoxelPos)) {
					ent.position = ent.position.plusZ(0.1f)
				}


				val oldPos = ent.position.toVoxelCoord
				ent.position = ent.position + ent.velocity.inVoxelsPerSecond * time.inSeconds
				val newPos = ent.position.toVoxelCoord
				if (ent.adjustedFootPos.z <= 0.001f) { ent.position = ObjectCoord(ent.position.x,ent.position.z,0.0011f + ent.boundingDimensions.z.inVoxels * 0.5f)}
				if (oldPos != newPos) {
					PELD.entityLocationOctree.remove(oldPos,ent)
					PELD.entityLocationOctree.set(newPos,ent)
				}
			} else {

			}
		}
	}

	override def queryResultAdded(t: TPhysicalEntity): Unit = {
		PELD.entityLocationOctree.set(t.position,t)
	}

	override def queryResultRemoved(t: TPhysicalEntity): Unit = {
		PELD.entityLocationOctree.remove(t.position,t)
	}
}
