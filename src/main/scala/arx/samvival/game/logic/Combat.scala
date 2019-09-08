package arx.samvival.game.logic

import arx.engine.lworld.{LEntity, LWorld, LWorldView}
import arx.samvival.game.entities.{CharacterInfo, DamageResult}
import arx.samvival.game.entities.Fields.CharacterInfo
import arx.Prelude._
import arx.core.introspection.FieldOperations._
import arx.samvival.game.events.GameEvents

object Combat {


	def applyDamage(character : LEntity, damageResults : Traversable[DamageResult], source : String)(implicit world : LWorld) = {
		if (damageResults.nonEmpty) {
			for (damageResult <- damageResults) {
				world.startEvent(GameEvents.DamageTaken(character, damageResult))

				world.modify(character, CharacterInfo.health reduceBy damageResult.amount, source)

				world.endEvent(GameEvents.DamageTaken(character, damageResult))

				dieIfNecessary(character)
			}
		}
	}

	def isDead(character : LEntity)(implicit world : LWorldView) = {
		val charInfo = character[CharacterInfo]
		charInfo.health.currentValue <= 0 || !charInfo.alive
	}

	def dieIfNecessary(character : LEntity)(implicit world : LWorld) = {
		implicit val view = world.view
		if (isDead(character)) {
			world.startEvent(GameEvents.Died(character))
			Movement.removeCharacterFromTile(character)
			world.modify(character, CharacterInfo.alive -> false, None)
			world.endEvent(GameEvents.Died(character))
		}
	}
}
