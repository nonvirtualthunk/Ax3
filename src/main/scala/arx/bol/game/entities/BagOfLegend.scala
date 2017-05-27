package arx.bol.game.entities

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.bol.game.entities.data.BagOfLegendData
import arx.engine.entity.GameEntity
import arx.engine.entity.TGameEntity

import scalaxy.loops._

class BagOfLegend(val ent : TGameEntity) {
	val ID = ent[BagOfLegendData]
}

object BagOfLegend {
	implicit def toID(bol : BagOfLegend): BagOfLegendData = bol.ID
	implicit def toEntity(bol : BagOfLegend) : TGameEntity = bol.ent
}
