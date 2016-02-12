package arx.tmp.game.logic.universe

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/14/13
 * Time: 4:24 PM
 * Created by nonvirtualthunk
 */

import arx.core.TDependable
import arx.core.gen.SimplexNoise

import scala.util.Random

trait TUniverseDataGenerator extends TDependable {
	var simplexGenerator : SimplexNoise = new SimplexNoise(1L)
	var randomGenerator = new Random(1L)

	def generateData ( universe : Universe )
}