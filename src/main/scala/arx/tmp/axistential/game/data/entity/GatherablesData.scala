package arx.axistential.game.data.entity

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/8/13
 * Time: 3:00 PM
 */

import arx.tmp.game.logic.descriptors.TDescriptor
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.data.TGameEntityAuxData

class GatherablesData extends TGameEntityAuxData {
	var gatherables = Set[Gatherable]()



}

case class Gatherable ( val entity : GameEntity , val toolRequired : Option[TDescriptor] , val structural : Boolean ){

}