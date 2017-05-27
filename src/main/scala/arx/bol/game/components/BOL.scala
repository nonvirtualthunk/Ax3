package arx.bol.game.components

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.bol.game.entities.BagOfLegend
import arx.bol.game.entities.data.BagOfLegendData
import arx.engine.world.World

import scalaxy.loops._

object BOL {
	protected val getInvEntQuery = memoize((w : World) => {
		w.auxDataQuery[BagOfLegendData]
	})
	protected val getBOL = memoize((w : World) => {
		new BagOfLegend(getInvEntQuery(w).results.head)
	})
	def bagOfLegend(world: World) = getBOL(world)
}
