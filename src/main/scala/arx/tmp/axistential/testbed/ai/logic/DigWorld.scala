package arx.axistential.testbed.ai.logic

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/5/13
 * Time: 4:33 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.axistential.ai.TAIAgent
import arx.axistential.ai.TAIGroup
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.testbed.ai.logic.DigWorld.SimplePhysicalEntity
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.world.World
import arx.tmp.game.logic.entities.core.GameEntity

class DigWorld extends World {
	val terrain = aux[TerrainData]

	val colony = new GameEntity with TAIGroup {}
	val worker1 = new SimplePhysicalEntity
	val worker2 = new SimplePhysicalEntity

	worker1.position = VoxelCoord.Center.toObjectCoord
	worker2.position = VoxelCoord.Center.toObjectCoord

	colony.addEntity(worker1)
	colony.addEntity(worker2)

	addEntity(worker1)
	addEntity(worker2)
	addEntity(colony)
}

object DigWorld {
	class SimplePhysicalEntity extends TPhysicalEntity with TAIAgent{

	}
}
