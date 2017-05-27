package arx.rog2.game.data.entity

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.traits.ArxEnum
import arx.core.traits.ArxEnumObject
import arx.engine.data.TGameEntityAuxData

import scalaxy.loops._

class Furniture extends TGameEntityAuxData {
	var flags = Set[ObjectFlag]()
}


class ObjectFlag(name : String) extends ArxEnum(name)
object ObjectFlag extends ArxEnumObject[ObjectFlag] {
	val Container = ObjectFlag("container")
	val Locked = ObjectFlag("locked")
	val Broken = ObjectFlag("broken")
	val Ruined = ObjectFlag("ruined")
}