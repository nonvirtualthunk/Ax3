package arx.bol.game.entities

/**
  * TODO: Add javadoc
  */

import arx.bol.game.entities.data.Behavior
import arx.bol.game.entities.data.CreatureData

object PlayerCharacter {
	def create(creatureType: CreatureArchetype) = {
		val entity = creatureType.create()

		val CD = entity[CreatureData]
		CD.behavior = Behavior.PlayerCharacter
		CD.hp *= 10
		CD.attackDamage *= 2
		CD.speed *= 2.0f

		entity
	}
}