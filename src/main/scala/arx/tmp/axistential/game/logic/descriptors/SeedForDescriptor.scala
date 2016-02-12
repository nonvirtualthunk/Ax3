package arx.axistential.game.logic.descriptors

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/2/15
 * Time: 10:35 AM
 */

import arx.axistential.game.archetypes.species.PlantSpecies
import arx.axistential.game.data.entity.plants.PlantSeedData
import arx.tmp.game.logic.descriptors.TEntityDescriptor
import arx.tmp.game.logic.entities.core.GameEntity

case class SeedForDescriptor (species : PlantSpecies) extends TEntityDescriptor {
	override def matchesEntity(gameEntity: GameEntity): Boolean = {
		gameEntity.auxDataOpt[PlantSeedData] match {
			case Some(psd) => psd.forSpecies.resolve() == species
			case None => false
		}
	}
	override def exampleMatch: GameEntity = GameEntity.Sentinel
}
