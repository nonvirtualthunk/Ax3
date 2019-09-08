package arx.samvival.game.logic

import arx.core.introspection.FieldOperations.ReduceableField
import arx.engine.lworld.{LEntity, LWorld}
import arx.samvival.game.entities.CharacterInfo
import arx.samvival.game.entities.{Fields, TurnState}
import arx.samvival.game.entities.Fields.{CharacterInfo, TurnState}
import arx.samvival.game.events.GameEvents._
import arx.core.introspection.FieldOperations._

object Turns {
	/**
	  * End the currently active faction's turn and start the turn of the next active faction, rolling over the overall
	  * turn if necessary
	  */
	def transitionFactionTurn()(implicit world : LWorld) : Unit = {
		implicit val view = world.view

		val turnState = view.worldData[TurnState]
		val oldFaction = turnState.activeFaction
		endFactionTurn(oldFaction)
		val factions = turnState.factionOrder
		val oldFactionIndex = factions.indexOf(oldFaction)
		val nextFactionIndex = (oldFactionIndex + 1) % factions.size
		val newFaction = factions(nextFactionIndex)

		// if we've wrapped around then we need to transition the overall turn
		if (nextFactionIndex <= oldFactionIndex) {
			endOverallTurn()
			startOverallTurn()
		}

		startFactionTurn(newFaction)
	}

	private def endFactionTurn(faction : LEntity)(implicit world : LWorld): Unit = {
		implicit val view = world.view

		world.addEvent(FactionTurnEnded(faction))
	}

	private def startFactionTurn(faction : LEntity)(implicit world : LWorld): Unit = {
		implicit val view = world.view

   	// collect all character entities to a list, we don't want it altering while we're looking at it
		val characterStore = view.dataStore[CharacterInfo]
		val characterEntities = characterStore.entities.toList
		for (charEnt <- characterEntities) {
			val charData = characterStore.get(charEnt)
			if (charData.faction == faction) {
				world.modify(charEnt, CharacterInfo.movePoints recoverToFull(), "turn start recovery")
				world.modify(charEnt, CharacterInfo.actionPoints recoverToFull(), "turn start recovery")

				world.addEvent(CharacterTurnStarted(charEnt))
			}
		}

		world.modifyWorld(TurnState.activeFaction -> faction, None)

		world.addEvent(FactionTurnStarted(faction))
	}

	private def endOverallTurn()(implicit world : LWorld) : Unit = {
		implicit val view = world.view
		val turnNumber = view.worldData[TurnState].turnNumber

		world.addEvent(TurnEnded(turnNumber))
	}

	private def startOverallTurn()(implicit world : LWorld) : Unit = {
		implicit val view = world.view

		val oldTurnNumber = view.worldData[TurnState].turnNumber


		world.modifyWorld(TurnState.turnNumber + 1, None)
		world.addEvent(TurnStarted(oldTurnNumber + 1))
	}
}
