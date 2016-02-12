package arx.tmp.game.logic.events

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/9/13
 * Time: 11:49 AM
 * To change this template use File | Settings | File Templates.
 */

import arx.tmp.game.logic.entities.core.GameEntity

case class EntityChangedEvent ( entity : GameEntity ) extends Event {

}
