package arx.tmp.game.networking.messages

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/6/15
 * Time: 10:52 AM
 */

import arx.tmp.game.networking.TNetworkedGameEntityAuxData

class AuxDataUpdateMessage {
	var entityId: Long = -1l
	var auxData: TNetworkedGameEntityAuxData = _
}
