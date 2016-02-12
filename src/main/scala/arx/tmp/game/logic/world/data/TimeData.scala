package arx.tmp.game.logic.world.data

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/8/14
 * Time: 8:53 AM
 */

import arx.Prelude._

class TimeData extends TWorldAuxData {
	var time = 1.second
	def turning = time.inSeconds.toInt / (1.turning.inSeconds.toInt)
}
