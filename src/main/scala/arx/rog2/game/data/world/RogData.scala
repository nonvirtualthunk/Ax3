package arx.rog2.game.data.world

/**
  * TODO: Add javadoc
  */

import arx.engine.data.TWorldAuxData
import arx.engine.entity.GameEntity
import arx.engine.entity.TGameEntity

class RogData extends TWorldAuxData {
	var player : TGameEntity = GameEntity.Sentinel
}
