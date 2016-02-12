package arx.tmp.game.logic.functions

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/11/15
 * Time: 8:40 AM
 */

import arx.core.Moddable
import arx.core.units.UnitOfTime
import arx.tmp.game.logic.world.data.TimeData

class TimeLimitedFunction[T](mainValue : Moddable[T], fallbackValue : Moddable[T], world : World, duration : UnitOfTime) extends Moddable[T]{
	val startTime = world.aux[TimeData].time
	override def resolve(): T = if (world.aux[TimeData].time - startTime < duration) { mainValue.resolve() } else { fallbackValue.resolve() }

	override def baseValue(): T = resolve()
}
