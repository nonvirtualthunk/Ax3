package arx.axistential.game.logic.ai

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/16/15
 * Time: 7:13 AM
 */

import arx.Prelude._
import arx.axistential.ai.AxistentialFloodSearchQuery
import arx.axistential.game.data.entity.AIData
import arx.axistential.game.data.entity.AnimalAIData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.entities.CreaturePropertiesData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.core.vec.coordinates.TMajorCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.world.data.TimeData

object AnimalAIUtil {
	def failedPathSearchDelay = GameSettings.ai.patches.failedPathSearchDelaySeconds.float.seconds
	def patchSearchMultiplier = GameSettings.ai.patches.patchSearchRadiusMultiplier.float
	def maxCostForNewPatchSearch = GameSettings.ai.patches.maxCostForNewPatchSearch.int


	def pickNewPatch (agent : TPhysicalEntity) = {
		val AD = agent.aux[AnimalAIData]
		val TD = agent.world.aux[TerrainData]
		val MD = agent.world.aux[TimeData]
		val examinationRadius = AD.patchSize
		val bestNewPatchLocOpt = if (MD.time - AD.lastPatchSearchFailure.getOrElse(-10000.seconds) < failedPathSearchDelay) {
			None
		} else {
			// Normally we try to find a new patch that is some multiplier more than examination radius away,
			// but when an area is nearly filled with patches, that will result in un-covered areas. We add a relaxation
			// wherein, if no patch can be found, we reduce our required minimum distance
			var relaxationsRemaining = 2
			var requiredMultiplier = patchSearchMultiplier
			var res : Option[List[VoxelCoord]] = None

			while (relaxationsRemaining >= 0 && res.isEmpty) {
				res = AxisSearcher.pathTo(AxistentialFloodSearchQuery(
					agent,
					examinationRadius * requiredMultiplier,
					maxCostForNewPatchSearch,
					(v1,v2,c,j) => distance(v1,v2),
					(v) => {
						AD.patches.fmin(p => distance(p.region.center,v))
					},
					(v) => TD.isSolid(v),
					(v) => TD.isSolid(v.x,v.y,v.z-1)
				))
				requiredMultiplier = 1.0f + (requiredMultiplier - 1.0f) * 0.5f
				relaxationsRemaining -= 1
			}
			res
		}

		bestNewPatchLocOpt match {
			case Some(bestNewPatchLoc) if bestNewPatchLoc.nonEmpty => {
				Some(AD.patchFor(bestNewPatchLoc.last,examinationRadius))
			}
			case _ => {
				AD.lastPatchSearchFailure = Some(MD.time)
				None
			}
		}
	}
	
	def isLocationThreatening (agent : TPhysicalEntity, targetPosition : TMajorCoord) = {
		agent.assertHasAuxData(classOf[AIData],classOf[CreaturePropertiesData])

		val AD = agent.aux[AIData]
		val CD = agent.aux[CreaturePropertiesData]
		// if this would involve moving closer to an enemy
		if (AD.detectedEnemies.exists(e => e.position.distanceTo(targetPosition) < e.position.distanceTo(agent.position))) {
			true
		} else {
			val observationRange = CD.maximumObservationRange
			// if we still remember an enemy we sighted near that location
			if (AD.rememberedEnemies.exists(mem => mem.lastObserved.distanceTo(targetPosition) < observationRange)) {
				true
			} else {
				false
			}
		}
	}
}
