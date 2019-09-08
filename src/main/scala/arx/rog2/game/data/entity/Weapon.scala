package arx.rog2.game.data.entity

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 6/5/17
  * Time: 7:37 AM
  */

import arx.Prelude._

import scalaxy.loops._
import arx.core.vec._
import arx.engine.data.TGameEntityAuxData

class Weapon extends TGameEntityAuxData {
	var attackDuration = MM(1.0f)
	var accuracy = MM(1.0f)
	var range = MM(1)

	var damageDice = MM(1)
	var damagePerDie = MM(6)
	var damageBonus = MM(0)
	var damageMultiplier = MM(1.0f)
}

class Item extends TGameEntityAuxData {
	var maxDurability = MM(25)
	var durabilityUsed = 0.0f
	var durabilityPerUse = MM(1)

	def durabilityRemaining = maxDurability - durabilityUsed
}

case class DieRoll(dieResult : Int, dieSize : Int)