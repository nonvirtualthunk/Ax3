package arx.axistential.game.events

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/18/15
 * Time: 5:23 PM
 */

import arx.axistential.game.entities.CreatureEntity
import arx.tmp.game.logic.event.TGameEvent

case class EnemiesLeftSightEvent ( subject_ : CreatureEntity, enemies : Set[CreatureEntity] ) extends TGameEvent {
	override def subject = Some(subject_)
	override def objects = enemies.toList
}
