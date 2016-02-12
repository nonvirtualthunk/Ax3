package arx.tmp.game.logic.world.data

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/17/15
 * Time: 7:55 AM
 */

import arx.engine.data.TWorldAuxData
import arx.engine.world.World
import arx.tmp.game.networking.NetworkedWorldDataDeltaHandler
import arx.tmp.game.networking.data.NetworkingWorldData

trait TNetworkedWorldData extends TWorldAuxData {
	def createDeltaHandler : NetworkedWorldDataDeltaHandler[_]

	override def onWorldAssigned(world: World): Unit = {
		super.onWorldAssigned (world)
		world.aux[NetworkingWorldData].worldDataChanged(this)
	}
}


abstract class NetworkedWorldDataUpdate(val fieldId : Int) {
	var forClass : Class[_] = _
}
