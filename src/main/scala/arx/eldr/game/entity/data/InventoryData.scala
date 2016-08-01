package arx.eldr.game.entity.data

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.engine.data.TGameEntityAuxData
import arx.engine.entity.GameEntity
import arx.engine.entity.TGameEntity
import scalaxy.loops._

class InventoryData extends TGameEntityAuxData {
	var heldEntities : Set[TGameEntity] = Set()
	var maxHeldEntities : Option[Int] = None
	var personalInventory : Boolean = false
}
