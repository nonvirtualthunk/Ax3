package arx.samvival.game.entities

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/28/18
  * Time: 8:14 AM
  */

import arx.Prelude._
import arx.core.introspection.{Field, Transformation}
import arx.core.macros.GenerateCompanion
import scalaxy.loops._
import arx.core.vec._
import arx.engine.data.TAuxData
import arx.engine.lworld.{FieldOperationModifier, LEntity, Modifier}

import scala.reflect.ClassTag
import scala.util.Random

@GenerateCompanion
class Item extends TAuxData {

}

@GenerateCompanion
class Weapon extends TAuxData {
	var attacks : Map[AnyRef, Attack] = Map()
}

@GenerateCompanion
class Attack extends TAuxData {
	var accuracyBonus : Int = 0
	var apCost : Int = 0
	var staminaCost : Int = 0
	var minRange : Int = 0
	var maxRange : Int = 1
	var damage : Map[AnyRef, DamageElement] = Map()
}

trait DamageType
object DamageType {

	case object Bludgeoning extends DamageType
	case object Piercing extends DamageType
	case object Slashing extends DamageType
}

case class DamageElement (damageDice : DicePool, damageType : DamageType)

case class Die (pips : Int)
case class DicePool (dice : List[Die]) {
	def roll (rand : Random) : DiceRoll = {
		DiceRoll(dice.map(d => rand.nextInt(d.pips) + 1))
	}
}
object DicePool {
	val D1 = DicePool(Die(1) :: Nil)
}
case class DiceRoll (results : List[Int])