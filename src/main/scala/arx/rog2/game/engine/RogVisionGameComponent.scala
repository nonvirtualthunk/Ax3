package arx.rog2.game.engine

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.core.units.UnitOfTime
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.entity.TGameEntity
import arx.engine.game.GameEngine
import arx.engine.game.components.GameComponent
import arx.modules.lighting.OctantShadowGrid
import arx.modules.lighting.Shadowcaster
import arx.rog2.engine.RogComponent
import arx.rog2.game.data.entity.Creature
import arx.rog2.game.data.entity.Physical
import arx.rog2.game.data.world.EntityOcclusionData
import arx.rog2.game.data.world.Terrain

import scalaxy.loops._

class RogVisionGameComponent(engine : GameEngine) extends GameComponent(engine) with RogComponent {

	val tmpGrid = new OctantShadowGrid
	var altGrid = new OctantShadowGrid

	def recomputeVision(ent : TGameEntity, CD : Creature): Unit = {
		altGrid.clear()

		val pos = ent[Physical].position
		val sightRangeV = CD.sightRange.inVoxels.toInt

		val occlusions = world[EntityOcclusionData].occlusionsRelativeTo(pos, sightRangeV, 1.0f)

		val T = world[Terrain]
		Shadowcaster.shadowcast(
			(dx,dy,dz) => {
				T.voxel(pos.x + dx,pos.y + dy,pos.z + dz).opacity match {
					case 0.0f => occlusions(Vec3i(dx,dy,dz))
					case o => o
				}
			},
			(dx,dy,dz,f) => altGrid(dx,dy,dz) = f,
			tmpGrid,
			sightRangeV,
			(f) => 0.0f,
			(dx,dy,dz) => dz.abs < 6
		)

		val tmp = CD.visionGrid
		CD.visionGrid = altGrid
		altGrid = tmp
		Noto.info("Updating vision")
	}

	override protected def updateSelf(dt: UnitOfTime): Unit = {
		recomputeVision(player, player[Creature])
	}

	def isVisibleTo(looker : TGameEntity, target : TGameEntity) = {
		if (!looker.hasAuxData[Creature]) {
			false
		} else {
			val range = looker[Creature].sightRange
			val PD = looker[Physical]
			val TPD = target[Physical]

			if (PD.position.distanceTo(TPD.position) > range) {
				false
			} else {
				val delta = TPD.position - PD.position
				val mag = delta.lengthSafe
				if (mag > 0.0f) {
					val terrain = world[Terrain]

					val deltaN = delta.normalize
					val start = PD.position
					(0.0f until mag by 0.25f).forall(f => {
						val p = start + deltaN * f
						terrain.voxel(p.x.toInt,p.y.toInt,p.z.toInt).material.opacity < 0.75f
					})
				} else {
					true
				}
			}
		}
	}
}
