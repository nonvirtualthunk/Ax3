package arx.axistential.game.logic.ai.juncturemappers

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/11/13
 * Time: 3:08 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.axistential.ai.Juncture
import arx.axistential.ai.TAIAgent
import arx.axistential.ai.TJunctureMapper
import arx.axistential.game.events.EnemiesSightedEvent
import arx.tmp.game.logic.event.TGameEvent

class EnemySightedJunctureMapper extends TJunctureMapper {
	def toJuncture(event: TGameEvent): Option[(TAIAgent, Juncture)] = event match {
		case EnemiesSightedEvent(sightedBy,enemies) => {
			Some( sightedBy -> EnemySightedJuncture(enemies) )
		}
		case _ => None
	}
}
