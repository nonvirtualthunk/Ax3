package arx.axistential.game.world

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 3/28/15
 * Time: 6:55 AM
 */

import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.data.world.TopologicalData
import arx.axistential.game.data.world.VegetationData
import arx.tmp.game.logic.world.data.TimeData

import scala.language.implicitConversions



object AxistentialWorld {
	implicit class AxistentialWorld(val world : WorldBase) extends AnyVal {
		def time = world.aux[TimeData].time
		def currentTime = time
		def terrainData = world.aux[TerrainData]
		def topologicalData = world.aux[TopologicalData]
		def vegetationData = world.aux[VegetationData]
	}
}
