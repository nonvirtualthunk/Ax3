package arx.samvival.game.logic

import arx.engine.lworld.{LEntity, LWorldView}
import arx.samvival.game.entities.{CharacterClass, Levels}

object Leveling {
	def mainCharacterClass(charEnt : LEntity)(implicit world : LWorldView): CharacterClass = {
		val levels = charEnt.data[Levels].classLevels
		if (levels.isEmpty) {
			CharacterClass.Classless
		} else {
			levels.maxBy(_._2)._1
		}
	}
}
