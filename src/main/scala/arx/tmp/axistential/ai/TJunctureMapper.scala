package arx.axistential.ai

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/11/13
 * Time: 3:03 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.tmp.game.logic.event.TGameEvent

trait TJunctureMapper {
	def toJuncture ( event : TGameEvent ) : Option[(TAIAgent,Juncture)]
}
