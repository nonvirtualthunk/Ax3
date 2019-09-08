package arx.samvival.game.logic

import java.util.concurrent.ThreadLocalRandom

import arx.engine.lworld.LWorld
import arx.samvival.game.entities.DicePoolBuilder._
import arx.samvival.game.logic.Randomizer.stdDicePool

import scala.util.Random

object Randomizer {
	val stdDicePool = 3 d 6

	def apply()(implicit world : LWorld) = {
		new Randomizer(new Random(world.currentTime.time))
	}
}


class Randomizer(val random : Random) {
	def stdRoll() = {
		stdDicePool.roll()(this)
	}
}