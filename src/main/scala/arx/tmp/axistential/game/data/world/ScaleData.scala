package arx.axistential.game.data.world

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/6/13
 * Time: 2:55 PM
 */

import arx.Prelude._
import arx.engine.data.TWorldAuxData

class ScaleData extends TWorldAuxData {
	val MountainPeak = 110.meters
	val CloudLevel = 85.meters
	val SnowLine = 70.meters
	val TreeLine = 40.meters
	val Hilltop = 10.meters
	val SeaLevel = 0.meters
	val ValleyFloor = (-50).meters

	val NormalRainfall = 220.cm
	val MaximumLightLevel = 64
	val BaseNutrientRegeneration = 25
}
