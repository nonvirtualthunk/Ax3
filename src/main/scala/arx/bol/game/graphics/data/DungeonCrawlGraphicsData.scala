package arx.bol.game.graphics.data

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.math.Recti
import arx.engine.graphics.data.TGraphicsData
import arx.graphics.GL
import arx.graphics.pov.TopDownCamera

import scalaxy.loops._

class DungeonCrawlGraphicsData extends TGraphicsData {
	val pov = new TopDownCamera(20.0f)

	def viewport = {
		val componentHeight = 800
		val curV = GL.maximumViewport
		Recti(0,curV.height - componentHeight,curV.width,componentHeight)
	}
}
