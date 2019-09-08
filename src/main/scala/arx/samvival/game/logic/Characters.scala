package arx.samvival.game.logic

import arx.core.math.Sext
import arx.engine.lworld.{LEntity, LWorld, LWorldView}
import arx.samvival.game.entities.{CharacterInfo, Faction}
import arx.samvival.game.entities.Fields.CharacterInfo
import arx.core.introspection.FieldOperations._

object Characters {
	def isPlayerCharacter (character : LEntity)(implicit view : LWorldView) : Boolean = {
		character[CharacterInfo].faction[Faction].player
	}

	def areFriendly(characterA : LEntity, characterB : LEntity)(implicit view : LWorldView) : Boolean = {
		characterA[CharacterInfo].faction == characterB[CharacterInfo].faction
	}

	def areEnemies(characterA : LEntity, characterB : LEntity)(implicit view : LWorldView) : Boolean = {
		! areFriendly(characterA, characterB)
	}

	def spendAP(character : LEntity, amount : Sext, source : String)(implicit world : LWorld) : Boolean = {
		implicit val view = world.view
		if (character[CharacterInfo].actionPoints.currentValue >= amount) {
			world.modify(character, CharacterInfo.actionPoints.reduceBy(amount, limitToZero = true), source)
			true
		} else {
			false
		}
	}
}
