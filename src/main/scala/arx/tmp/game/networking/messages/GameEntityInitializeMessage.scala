package arx.tmp.game.networking.messages

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/6/15
 * Time: 10:52 AM
 */

import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.data.TGameEntityAuxData

case class GameEntityInitializeMessage(entity : GameEntity, auxData : List[TGameEntityAuxData])
