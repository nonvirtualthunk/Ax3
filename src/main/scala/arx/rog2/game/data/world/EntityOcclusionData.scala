package arx.rog2.game.data.world

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.datastructures.QueryBackedSparseVoxelOctree
import arx.core.datastructures.SparseVoxelOctree
import arx.core.vec.Cardinals
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.data.TWorldAuxData
import arx.engine.entity.TGameEntity
import arx.rog2.game.data.entity.Physical

import scalaxy.loops._

class EntityOcclusionData extends TWorldAuxData {
	val entities = new SparseVoxelOctree[TGameEntity](8)

	def occlusionsRelativeTo(v : VoxelCoord, range : Int, cutoff : Float) = {
		var occlusions = Map[ReadVec3i, Float]().withDefaultValue(0.0f)

		for (otherEnt <- entities.getInVolume(v, range)) {
			val OPD = otherEnt[Physical]
			val relPos = OPD.position - v
			if (OPD.effectiveOpacity >= 1.0f) {
				val dim = OPD.dimensions.inVoxels.round
				for (dx <- relPos.x until relPos.x + dim.x optimized;
						dy <- relPos.y until relPos.y + dim.y optimized;
						dz <- relPos.z until relPos.z + dim.z optimized){
					val v = Vec3i(dx,dy,dz)
					occlusions += v -> 1.0f
				}
			} else if (OPD.effectiveOpacity > cutoff) {
				for (dz <- 0 until OPD.dimensions.z.inVoxels.toInt ; q <- 0 until 8) {
					val c = Cardinals.expandedDirVec2d(q)
					val f = relPos + c + Vec3i(0,0,dz)
					if (f.lengthSafe > relPos.lengthSafe) {
						//				val rawPcnt = c.normalizeSafe.dot(playerRelPos.normalizeSafe) * (f.lengthSafe - playerRelPos.lengthSafe).min(1.0f)
						//				val rawPcnt = powf(f.normalizeSafe.dot(playerRelPos.normalizeSafe),3.0f) * c.dot(playerRelPos.normalizeSafe)
						val rawPcnt = c.normalizeSafe.dot(relPos.normalizeSafe)
						val pcnt = if (rawPcnt < 0.0f) {
							rawPcnt
						} else {
							powf(rawPcnt,3.0f) * OPD.effectiveOpacity
						}
						if (pcnt > 0.1f) {
							occlusions += f -> occlusions(f).max(pcnt)
						}
					}
				}
			}
		}

		occlusions
	}


	/**
	  * Returns a map with keys being absolute positions, values being whether or not there is something there
	  */
	def solidVoxelsNear(v : VoxelCoord, range : Int) = {
		var occlusions = Map[ReadVec3i, Boolean]().withDefaultValue(false)

		for (otherEnt <- entities.getInVolume(v, range)) {
			val OPD = otherEnt[Physical]
			val absPos = OPD.position
			if (OPD.solid) {
				val dim = OPD.dimensions.inVoxels.round
				for (dx <- absPos.x until absPos.x + dim.x optimized;
					  dy <- absPos.y until absPos.y + dim.y optimized;
					  dz <- absPos.z until absPos.z + dim.z optimized){
					val v = Vec3i(dx,dy,dz)
					occlusions += v -> true
				}
			}

		}

		occlusions
	}
}
