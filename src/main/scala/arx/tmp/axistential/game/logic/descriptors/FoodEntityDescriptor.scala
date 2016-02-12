package arx.axistential.game.logic.descriptors

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 9/24/14
 * Time: 7:35 AM
 */

import arx.axistential.game.archetypes.item.ItemArchetype
import arx.axistential.game.data.entity.FoodData
import arx.tmp.game.logic.descriptors.TEntityDescriptor
import arx.tmp.game.logic.entities.core.GameEntity

class FoodEntityDescriptor extends TEntityDescriptor {
	override def matchesEntity(gameEntity: GameEntity): Boolean = {
		gameEntity.auxDataOpt[FoodData] match {
			case Some(fd) => fd.calories > 0
			case None => false
		}
	}
	override def exampleMatch: GameEntity = {
		ItemArchetype.allArchetypes.find(a => this.matchesEntity(a.exampleInstance)) match {
			case Some(a) => a.exampleInstance
			case None => GameEntity.Sentinel
		}
	}
}
