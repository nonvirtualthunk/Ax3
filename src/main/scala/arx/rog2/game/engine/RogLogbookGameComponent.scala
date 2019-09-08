package arx.rog2.game.engine

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 5/27/17
  * Time: 10:16 AM
  */

import arx.Prelude._

import scalaxy.loops._
import arx.core.vec._
import arx.engine.entity.TGameEntity
import arx.engine.event.GameEvent
import arx.engine.game.GameEngine
import arx.engine.game.components.GameComponent
import arx.rog2.engine.RogComponent
import arx.rog2.game.data.world.Logbook
import arx.rog2.game.events.RogGameEvent

class RogLogbookGameComponent(ge : GameEngine) extends GameComponent(ge) with RogComponent {

	gameEvents.onEvent {
		case e : RogGameEvent =>
			val msg = e.logMessage
			world[Logbook].messages ::= msg
	}

}
