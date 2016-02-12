package arx.tmp.game.logic.entities.data

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 4/2/15
 * Time: 8:07 AM
 */

import arx.tmp.game.logic.entities.core.THasAuxData

trait TAuxData {
	def onAssignedToObject ( entity : THasAuxData[_] ) {}
}