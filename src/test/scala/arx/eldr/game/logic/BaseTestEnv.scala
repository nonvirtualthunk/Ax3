package arx.eldr.game.logic

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.units.UnitOfTime
import arx.core.vec.Vec3i
import arx.eldr.game.archetypes.Material
import arx.eldr.game.entity.data.AIAgentData
import arx.eldr.game.entity.data.CubeShape
import arx.eldr.game.entity.data.InventoryData
import arx.eldr.game.entity.data.PhysicalData
import arx.eldr.game.logic.ai.AIGameComponent
import arx.eldr.game.logic.ai.AIGroupData
import arx.eldr.game.world.data.Terrain
import arx.engine.entity.GameEntity
import arx.engine.event.EventBus
import arx.engine.game.GameEngine
import arx.engine.world.World

import scalaxy.loops._

class BaseTestEnv {
	val world = new World
	val engine = new GameEngine(world, new EventBus)

	world[Terrain].setMaterialsInRegion(VoxelRegion(VCR(-10,-10,-1),VCR(10,10,-1)),Material.withName("stone"))

	val creature = new GameEntity("Testman").withData[PhysicalData].withData[AIAgentData]
	creature[PhysicalData].position = VCR(-5,0,0)
	creature[AIAgentData].goals = Set()
	world addEntity creature

	val stockpile = new GameEntity("Test-Pile")
	stockpile[PhysicalData].position = VCR(0,5,0)
	stockpile[PhysicalData].shape = CubeShape(Vec3i.One)
	stockpile[InventoryData].maxHeldEntities = None

	val group = new GameEntity("TestGroup").withData[AIGroupData]
	group.entities += creature
	group.entities += stockpile
	creature.groups += group


	def advanceUntil(b : => Boolean, timeout : UnitOfTime): Unit = {
		var steps = timeout.inSeconds / 0.01666667f
		while (steps >= 0 && ! b) {
			engine.updateSerial(0.016667f, 1)
			steps -= 1
		}
	}

	def advance(timeout : UnitOfTime) : Unit = {
		var steps = timeout.inSeconds / 0.01666667f
		while (steps >= 0) {
			engine.updateSerial(0.016667f, 10)
			steps -= 10
		}
	}
}
