package arx.axistential.game.components

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/11/13
 * Time: 3:00 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.axistential.ai.AIGameComponent
import arx.core.units.UnitOfTime

class JunctureComponent extends GameEngineComponent {
	dependencies = List( classOf[AIGameComponent] )
	val aiComponent = reify[AIGameComponent]

	def update(time: UnitOfTime): Unit = {

	}
}
