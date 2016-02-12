package arx.axistential.game.logic.general

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/31/14
 * Time: 1:13 PM
 */

import arx.axistential.game.archetypes.species.PlantSpecies
import arx.axistential.game.data.entity.plants.PlantData
import arx.axistential.game.data.entity.plants.PlantSeedData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.core.vec.coordinates.TMajorCoord
import arx.engine.world.World
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.world.data.TimeData

object PlantLogic {
	def seedFor (ent : GameEntity) : Option[PlantSpecies] = {
		ent.auxDataOpt[PlantSeedData].map(_.forSpecies)
	}

	/**
	 * Creates a new plant from the given spcies and places it in the given world at the given location.
	 * If a seed is provided it uses that seed for any specific creation steps that may entail, then destroys
	 * the seed and removes it from the world.
	 */
	def createPlant (ofSpecies : PlantSpecies, inWorld : World, atLocation : TMajorCoord, fromSeed : Option[TPhysicalEntity]) = {
		val inst = ofSpecies.createInstance
		inst.aux[PlantData].born = inWorld.aux[TimeData].time
		inst.position = atLocation.toObjectCoordFoot.plusZ(0.0001f).plusZ(inst.boundingDimensions.z.inVoxels * 0.5f)

		inWorld.addEntity(inst)
		for (seed <- fromSeed) { EntityLogic.destroyEntity(seed) }
		inst
	}
}
