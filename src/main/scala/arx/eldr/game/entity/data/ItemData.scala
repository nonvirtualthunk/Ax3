package arx.eldr.game.entity.data

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.core.traits.ArxEnum
import arx.core.traits.ArxEnumObject
import arx.core.traits.TArxEnum
import arx.engine.data.TGameEntityAuxData
import arx.engine.entity.TGameEntity

import scalaxy.loops._

class ItemData extends TGameEntityAuxData {
	var flags = Set[ItemFlag]()
	var durability = MM(10.0f)
	var createdFrom : List[TGameEntity] = Nil
}


class ItemFlag(nomen : String) extends ArxEnum(nomen) {

}

object ItemFlag extends ArxEnumObject[ItemFlag] {
	val SharpEdge = ItemFlag("cutting edge")
	val DurableEdge = ItemFlag("durable edge")
	val Fiber = ItemFlag("fiber")
	val Cloth = ItemFlag("cloth")
}