package arx.samvival.game.logic

import arx.Prelude.toArxList
import arx.ai.search.{Path, PathStep}
import arx.application.Noto
import arx.core.vec.coordinates.{AxialVec, AxialVec3}
import arx.engine.lworld.{EntityDataStore, LEntity, LWorld, LWorldView}
import arx.samvival.game.entities.Fields.{Physical, Tile}
import arx.samvival.game.entities._
import arx.core.introspection.FieldOperations._
import arx.core.math.Sext
import arx.samvival.game.events.GameEvents.{EntityMoved, EntityPlaced}
import arx.samvival.game.entities.Fields.CharacterInfo


object Movement {

	def moveCostTo(character : LEntity, position : AxialVec3, tileStore : EntityDataStore[Tile], terrainStore : EntityDataStore[Terrain], vegetationStore : EntityDataStore[Vegetation], physicalStore : EntityDataStore[Physical]) : Option[Sext] = {
		val tileEnt = Tiles.tileAt(position)
		tileStore.getOpt(tileEnt) match {
			case Some(tileData) =>
				if (!tileData.entities.exists(e => physicalStore.getOpt(e).exists(p => p.occupiesHex))) {
					val terrain = terrainStore.get(tileEnt)
					val vegetation = vegetationStore.get(tileEnt)

					Some(terrain.moveCost + vegetation.moveCost)
				} else {
					None
				}
			case None => None
		}
	}

	def moveCostTo(character : LEntity, position : AxialVec3)(implicit view : LWorldView) : Option[Sext] = {
		moveCostTo(character,position, view.dataStore[Tile], view.dataStore[Terrain], view.dataStore[Vegetation], view.dataStore[Physical])
	}


	private def setCharacterPosition(character : LEntity, position : AxialVec3)(implicit world : LWorld) : Boolean = {
		implicit val view = world.view
		val tile = Tiles.tileAt(position)
		if (tile[Tile].entities.isEmpty) {
			val prevTile = Tiles.tileAt(character[Physical].position)
			if (prevTile[Tile].entities.contains(character)) {
				world.modify(prevTile, Tile.entities - character, None)
			}

			world.modify(character, Physical.position -> position, None)
			world.modify(tile, Tile.entities -> Set(character), None)
			true
		} else {
			false
		}
	}

	def placeCharacterAt(character : LEntity, position : AxialVec3)(implicit world : LWorld): Boolean = {
		implicit val view = world.view

		if (setCharacterPosition(character, position)) {
			world.addEvent(EntityPlaced(character, position))
			true
		} else {
			false
		}
	}

	def removeCharacterFromTile(character : LEntity)(implicit world : LWorld) : Boolean = {
		implicit val view = world.view

		val tile = Tiles.tileAt(character[Physical].position)
		if (tile[Tile].entities.contains(character)) {
			world.modify(tile, Tile.entities - character, None)
			true
		} else {
			Noto.warn("Tried to remove character from tile, but it was not present")
			false
		}
	}

	def moveCharacterOnPath(character : LEntity, path : Path[AxialVec3])(implicit world : LWorld) : Boolean = {
		implicit val view = world.view

		val physical = character[Physical]
		val characterData = character[CharacterInfo]

		if (path.steps.headOption.exists(h => h.node == physical.position)) {
			for ((from,to) <- path.steps.map(p => p.node).sliding2) {
				moveCostTo(character, to) match {
					case Some(moveCost) if characterData.movePoints.currentValue >= moveCost =>
						val positionChangedSuccessfully = setCharacterPosition(character, to)
						if (!positionChangedSuccessfully) {
							return false
						}

						world.modify(character, CharacterInfo.movePoints reduceBy moveCost, "movement")
						world.addEvent(EntityMoved(character, from, to))
					case _ => return false
				}
			}
			true
		} else {
			Noto.error("Path started at hex other than the character's current hex")
			false
		}
	}

	def subPath(character : LEntity, path : Path[AxialVec3], costLimit : Sext)(implicit world : LWorld) : Path[AxialVec3] = {
		implicit val view = world.view

		path.steps match {
			case head :: _ =>
				var cost = Sext(0)

				var resultPath = Vector[PathStep[AxialVec3]](head)
				path.steps.sliding2.foreach{ case (from, to) => {
					moveCostTo(character, to.node) match {
						case Some(singleStepCost) =>
							if (cost + singleStepCost > costLimit) {
								return Path(resultPath.toList)
							} else {
								resultPath :+= to
								cost += singleStepCost
							}
						case None =>
							Noto.error("Path encountered with non-moveable path step")
							return Path(resultPath.toList)
					}
				}}
				Path(resultPath.toList)
			case _ =>
				path
		}
	}
}
