package arx.axistential.game.logic.ai.juncturemappers

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/11/13
 * Time: 3:09 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.axistential.ai.AI
import arx.axistential.ai.Juncture
import arx.axistential.ai.TAIAgent
import arx.axistential.game.entities.CreatureEntity
import arx.core.vec.coordinates.ObjectCoord
import arx.engine.world.World

case class EnemySightedJuncture ( enemies : Traversable[CreatureEntity] ) extends Juncture {
	val sightedAt : Map[CreatureEntity,ObjectCoord] = enemies.map( e => e -> e.position ).toMap

	def priority: Int = {
		AI.Priority.ImminentDanger
	}

	//Now, this definition of stillApplies is broader than the initial sighting might indicate. Naively, you could
	//say, as soon as the enemy is no longer visible to the one who sighted it, this doesn't apply, but that would
	//not work well. It would lead to ping-ponging if the enemy were stationary. Workers would continuously approach,
	//see the enemy, retreat, return, etc.
	def stillApplies(world: World, agent: TAIAgent): Boolean = {
		enemies.exists( enemy => enemy.alive && enemy.position.distanceTo( sightedAt(enemy) ) < 8.meters )
	}
}
