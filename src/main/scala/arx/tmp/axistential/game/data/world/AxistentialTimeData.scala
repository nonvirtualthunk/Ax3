package arx.axistential.game.data.world

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/10/15
 * Time: 1:29 PM
 */

import arx.Prelude._
import arx.core.units.UnitOfTime
import arx.tmp.game.logic.world.data.TimeData

import scala.language.implicitConversions

class AxistentialTimeData(timeData:TimeData) {
	def season = AxistentialTimeData.seasonFromTime(timeData.time)
}


object AxistentialTimeData {
	implicit def toAxisTD (td : TimeData) : AxistentialTimeData = new AxistentialTimeData(td)

	private val seasons = Array(Season.Spring,Season.Summer,Season.Autumn,Season.Winter)
	def seasonFromTime (time : UnitOfTime) = {
		val seasonIndex = time.inSeconds.toInt / (1.sowing.inSeconds.toInt)
		seasons(seasonIndex % seasons.size)
	}
}