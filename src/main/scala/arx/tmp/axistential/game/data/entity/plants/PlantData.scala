package arx.axistential.game.data.entity.plants

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/30/14
 * Time: 3:44 PM
 */

import arx.axistential.game.data.entity.LivingThingData
import arx.core.representation.InformationLevel.InformationLevel
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.core.TGameBase
import arx.tmp.game.logic.entities.data.TGameEntityAuxData

class PlantData extends LivingThingData with TGameEntityAuxData {

	override def informationPairs(level: InformationLevel): Map[String, Any] = EmptyMap

	override def onAssignedToEntity(entity: TGameBase): Unit = entity match {
		case ent : GameEntity => born = ent.worldTime
		case _ =>
	}
}
