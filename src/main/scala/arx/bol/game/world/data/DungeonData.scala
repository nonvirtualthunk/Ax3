package arx.bol.game.world.data

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.bol.game.entities.PlayerCharacter
import arx.core.vec.ReadVec2i
import arx.engine.data.TWorldAuxData
import arx.engine.entity.TGameEntity

import scalaxy.loops._

class DungeonData extends TWorldAuxData {
	val activeDungeon = new Dungeon
}

class Dungeon {
	val length = 500
	val heightmap = Array.ofDim[Int](length)
}