package arx.tmp.game.logic.universe.data

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/15/14
 * Time: 9:39 AM
 */

import arx.tmp.game.logic.mythology.Pantheon
import arx.tmp.game.logic.universe.TUniverseData

class DeityData extends TUniverseData {
	var pantheons = List[Pantheon]()

	def pantheonWithName (name : String) = pantheons.find(p => p.name == name)
}
