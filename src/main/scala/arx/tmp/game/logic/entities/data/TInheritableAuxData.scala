package arx.tmp.game.logic.entities.data

import arx.core.introspection.CopyAssistant
import arx.tmp.game.logic.entities.core.GameEntity

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/6/14
 * Time: 7:24 AM
 */

trait TInheritableAuxData extends TGameEntityAuxData {
	def copyToInstance ( entity : GameEntity ) = {
		val inst = CopyAssistant.copyShallow(this)
		entity.manualAddAuxData(inst)
	}
}
