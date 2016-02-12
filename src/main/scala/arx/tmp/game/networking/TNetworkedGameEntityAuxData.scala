package arx.tmp.game.networking

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/9/15
 * Time: 12:09 PM
 */

import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.core.THasAuxData
import arx.tmp.game.logic.entities.data.TGameEntityAuxData
import arx.tmp.game.networking.data.NetworkingWorldData

trait TNetworkedGameEntityAuxData extends TGameEntityAuxData {
	@transient var entity : GameEntity = _
	def frequency = TNetworkedGameEntityAuxData.Normal


	protected def fieldModified (): Unit = {
		if (entity != null) {
			if (!entity.world.isSentinel) {
				entity.world.aux[NetworkingWorldData].auxDataChanged(this)
			}
		}
	}

	override def onAssignedToObject(assignedTo: THasAuxData[_]): Unit = {
		super.onAssignedToObject (assignedTo)
		assignedTo match {
			case ent : GameEntity => entity = ent
				// This was a throw, but inheritable data can go to archetype, then passed down to entity
			case _ => //throw new IllegalStateException(s"NetworkedAuxData must be assigned only to GameEntity's, instead assigned to $assignedTo")
		}
	}
}

object TNetworkedGameEntityAuxData {
	val Frequent = 0
	val Normal = 10
	val Infrequent = 100
}