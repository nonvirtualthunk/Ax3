package arx.axistential.game.archetypes

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/9/13
 * Time: 10:55 AM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.core.traits.TSentinel
import arx.core.traits.TSentinelable
import arx.tmp.game.logic.entities.core.GameArchetype
import arx.tmp.game.logic.world.util.TIdentifiable

case class Covering ( archetype : GameArchetype, state : Int = 0 ) extends TIdentifiable with TSentinelable {
	override val identifier: String = archetype.name + state

	var name = archetype.displayName + " covering"
	def stateContains(v : Int) = state.isBitSet(v)
}

object Covering {
	val Sentinel : Covering = new Covering(Material.Sentinel,0) with TSentinel{
		name = "Sentinel Covering"
		protected def readResolve : Object = Covering.Sentinel
	}

	val CroppedShort = 1 << 0

}