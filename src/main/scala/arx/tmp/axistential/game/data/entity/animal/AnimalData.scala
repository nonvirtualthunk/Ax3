package arx.axistential.game.data.entity.animal

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 4/11/15
 * Time: 10:35 AM
 */

import arx.axistential.game.data.entity.LivingThingData
import arx.core.representation.InformationLevel._
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.core.TGameBase
import arx.tmp.game.logic.entities.data.TGameEntityAuxData

class AnimalData extends LivingThingData with TGameEntityAuxData {
	override def informationPairs(level: InformationLevel): Map[String, Any] = EmptyMap

	override def onAssignedToEntity(entity: TGameBase): Unit = entity match {
		case ent : GameEntity => born = ent.worldTime
		case _ =>
	}
}
