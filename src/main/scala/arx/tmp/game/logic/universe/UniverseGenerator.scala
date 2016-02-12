package arx.tmp.game.logic.universe

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/3/14
 * Time: 7:29 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.Dependency
import arx.core.gen.SimplexNoise

import scala.util.Random

class UniverseGenerator {
	def generateUniverse() : Universe = {
		val simplexGenerator : SimplexNoise = new SimplexNoise(System.currentTimeMillis())
		val randomGenerator = new Random(System.currentTimeMillis())

		Noto.info("creating universe")
		val universe = new Universe
		var generators = ReflectionAssistant.instancesOfSubtypesOf[TUniverseDataGenerator]
		generators = Dependency.recursiveResolveDependables(generators).ofType[TUniverseDataGenerator]
		generators = Dependency.topologicalSort(generators)

		for ( generator <- generators ) {
			generator.simplexGenerator = simplexGenerator
			generator.randomGenerator = randomGenerator
			generator.generateData(universe)
		}

		Noto.info("universe created")
		universe
	}
}
