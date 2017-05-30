package arx.rog2.game.data.world

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 5/27/17
  * Time: 10:15 AM
  */

import arx.Prelude._

import scalaxy.loops._
import arx.core.vec._
import arx.engine.data.TWorldAuxData
import arx.rog2.game.events.Rog

class Logbook extends TWorldAuxData {
	var messages = List[Rog.LogMessage]()
}
