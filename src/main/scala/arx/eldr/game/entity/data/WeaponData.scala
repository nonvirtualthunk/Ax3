package arx.eldr.game.entity.data

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.Moddable
import arx.core.traits.ArxEnum
import arx.core.traits.ArxEnumObject
import arx.core.traits.TArxEnum
import arx.engine.data.TGameEntityAuxData

import scalaxy.loops._

class WeaponData extends TGameEntityAuxData {
	var speed = Moddable(0.0f)
	var accuracy = Moddable(0.0f)

	var slashDamage = Moddable(0.0f)
	var bludgeonDamage = Moddable(0.0f)
	var pierceDamage = Moddable(0.0f)

	var extraDamage = Moddable(Map[DamageType, Float]())
}


class DamageType(nomen : String) extends ArxEnum(nomen) {}
object DamageType extends ArxEnumObject[DamageType] {
	val Bludgeoning = DamageType("bludgeoning")
	val Slashing = DamageType("slashing")
	val Piercing = DamageType("piercing")
	val Fire = DamageType("fire")
}