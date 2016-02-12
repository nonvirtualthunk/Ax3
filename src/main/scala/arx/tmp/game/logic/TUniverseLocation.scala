package arx.tmp.game.logic

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/10/13
 * Time: 4:13 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.core.traits.TSentinel
import arx.core.vec.coordinates.UniverseCoord
import arx.tmp.game.logic.entities.core.GameEntity

trait TUniverseLocation {
	var universePosition = UniverseCoord.Sentinel
	def radius = 5.universe
}
object UniverseLocation {
	object Sentinel extends GameEntity with TUniverseLocation with TSentinel
}