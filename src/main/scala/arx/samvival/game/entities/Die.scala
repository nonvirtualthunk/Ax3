package arx.samvival.game.entities

import arx.samvival.game.logic.Randomizer

import scala.util.Random


case class Die (pips : Int)
case class DicePool (dice : List[Die]) {
	def roll ()(implicit randomizer : Randomizer) : DiceRoll = {
		rollWith(randomizer.random)
	}

	def rollWith (rand : Random) : DiceRoll = {
		DiceRoll(dice.map(d => rand.nextInt(d.pips) + 1))
	}

	override def toString() = {
		dice.groupBy(d => d.pips).map { case (pips, matchingDice) => s"${matchingDice.size}d$pips" }.mkString(" + ")
	}
}
object DicePool {
	val D1 = DicePool(Die(1) :: Nil)
	def apply(n : Int) = DicePoolBuilder(n)
}
case class DicePoolBuilder(n : Int) {
	def d(pips : Int) = new DicePool((0 until n).map(_ => new Die(pips)).toList)
}
object DicePoolBuilder {
	implicit def fromInt(n : Int) = new DicePoolBuilder(n)
}
case class DiceRoll (results : List[Int]) {
	def total = results.sum
}