package arx.axistential.ai

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 4/5/15
 * Time: 8:33 AM
 */

import arx.axistential.game.components.EnemyAwarenessComponent
import arx.core.units.UnitOfTime

class AISupportComponent extends GameEngineComponent {
	dependencies = List(classOf[EnemyAwarenessComponent])

	override def update(time: UnitOfTime): Unit = {}
}
