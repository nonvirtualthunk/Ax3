package arx.axistential.ai

import arx.axistential.game.logic.ai.juncturemappers.EnemySightedJuncture
import arx.engine.world.World

/**
 * Trait for a goal that is not specific to an individual location. This would include things
 * like EatWhenHungry or FleeFromEnemies, as contrasted with BuildVoxel or the like. This is
 * primarily a marker trait, but has the additional feature of interacting differently with
 * location-specific interruptions like EnemySighted.
 */
trait TNonLocationConstrainedGoal extends Goal {
	override protected def interruptionApplies(interruption: AIReason, world: World, agent: TAIAgent): Boolean = {
		interruption match {
			case es : EnemySightedJuncture => {
//				agent match {
//					case pe : TPhysicalEntity =>
//						es.enemies.exists(e => pe.position.scalarDistanceTo(e) < )
//					case _ => false
//				}
				false
			}
			case _ => super.interruptionApplies (interruption, world, agent)
		}
	}
}
