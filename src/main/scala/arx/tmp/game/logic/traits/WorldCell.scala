package arx.tmp.game.logic.traits

import arx.tmp.game.logic.world.TerrainFeature

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/16/11
 * Time: 3:50 PM
 * To change this template use File | Settings | File Templates.
 */

trait WorldCell {
	var features = List[TerrainFeature]()
}