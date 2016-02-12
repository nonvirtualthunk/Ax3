package arx.tmp.game.logic.mythology.descriptors

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/15/14
 * Time: 7:47 AM
 */

import arx.tmp.game.logic.descriptors.TEntityDescriptor
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.mythology.MythologicalEntity
import arx.tmp.game.logic.mythology.MythologicalEntityData

trait TMythologicalEntityDescriptor extends TEntityDescriptor {
	override def matchesEntity(gameEntity: GameEntity): Boolean = gameEntity.auxDataOpt[MythologicalEntityData] match {
		case Some(med) => matchesMythologicalEntity(med)
		case None => false
	}
	
	def matchesMythologicalEntity(entity : MythologicalEntityData) : Boolean

	override def exampleMatch: GameEntity = new MythologicalEntity
}
