package arx.tmp.game.logic.civilization

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/3/14
 * Time: 5:34 PM
 */

import arx.tmp.game.logic.civilization.Civilization.Relationship
import arx.tmp.game.logic.entities.core.GameEntity

class Civilization extends GameEntity {
	var aggression = 0f
	var intellect = 0f
	var religiosity = 0f

	var foodProduction = 0f
	var metalProduction = 0f
	var woodProduction = 0f

	var militaryStrength = 0f
	var wealth = 0f
	var food = 0f

	var relationships = Map[Civilization,List[Relationship]]()
}

object Civilization {
	class Founder extends GameEntity {

	}

	class Relationship(val from : Civilization,val to : Civilization) extends Serializable {

	}
}