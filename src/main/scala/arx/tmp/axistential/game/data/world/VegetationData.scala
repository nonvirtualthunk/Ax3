package arx.axistential.game.data.world

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/30/14
 * Time: 4:13 PM
 */

import arx.engine.data.TWorldAuxData

class VegetationData extends TWorldAuxData {
	/** Used as a marker to ensure that plants are correctly set up, exactly once, on world creation */
	var initialized = false
}
