package arx.bol.game.entities.data

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.traits.ArxEnum
import arx.core.traits.ArxEnumObject
import arx.engine.data.TGameEntityAuxData

import scalaxy.loops._

class CreatureData extends TGameEntityAuxData {
	var speed = 0.5.m_s

	var currentAction = ActionType.Walk

	var behavior = Behavior.AttackWhenNear

	var hp = 10

	var actionProgress = 0.0f

	var attackTime = 1.seconds
	var attackDamage = 1
}

class ActionType(name : String) extends ArxEnum(name) {

}
object ActionType extends ArxEnumObject[ActionType] {
	val Attack = ActionType("Attack")
	val Walk = ActionType("Walk")
	val Wait = ActionType("Wait")
}


class Behavior(name : String) extends ArxEnum(name) {

}
object Behavior extends ArxEnumObject[Behavior] {
	val AttackWhenNear = new Behavior("AttackWhenNear")
	val PlayerCharacter = new Behavior("PlayerCharacter")
}

//class CreatureType(name : String) {
//
//}
//object CreatureType {
//	val Human = new CreatureType("human")
//	val Skeleton = new CreatureType("skeleton")
//}