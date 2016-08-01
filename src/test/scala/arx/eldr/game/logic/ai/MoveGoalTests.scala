package arx.eldr.game.logic.ai

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.axistential.game.logic.ai.goals.MoveGait
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.vec.coordinates.VoxelCoord
import arx.eldr.game.archetypes.Material
import arx.eldr.game.entity.data.AIAgentData
import arx.eldr.game.entity.data.PhysicalData
import arx.eldr.game.logic.BaseTestEnv
import arx.eldr.game.logic.ai.goals.MoveGoal
import arx.eldr.game.world.data.Terrain
import arx.engine.entity.GameEntity
import arx.engine.event.EventBus
import arx.engine.game.GameEngine
import arx.engine.world.World
import org.scalatest.FlatSpec
import scalaxy.loops._

class MoveGoalTests extends FlatSpec {

	def testEnv() = new BaseTestEnv {
		engine.addComponent[AIGameComponent]
	}
	
	"Simple movement goal" should "cause an entity to move from one position to another" in {
		val env = testEnv()
		import env._

		val endPos = VCR(5,0,0)

		val goal = new MoveGoal(VoxelRegion(endPos),MoveGait.Walk)
		creature.goals += goal

		engine.updateSerial(0.0166f, 60*10)

		assert(goal.interruptedBy.isEmpty)
		assert(creature.position == endPos)
		assert(goal.complete)
		assert(creature.goals.isEmpty)
	}

	"No-op movement goal" should "complete immediately, without complications" in {
		val env = testEnv()
		import env._

		val endPos = creature.position
		val goal = new MoveGoal(VoxelRegion(endPos),MoveGait.Walk)
		creature.goals += goal
		engine.updateSerial(0.016667f, 10)

		assert(creature.position == endPos)
		assert(goal.interruptedBy.isEmpty && goal.complete)
	}

	"Move goal requiring going around a wall" should "find a proper route and move around" in {
		val env = testEnv()
		import env._

		val wallRegion = VoxelRegion(VCR(0,-5,0),VCR(0,5,10))
		world[Terrain].setMaterialsInRegion(wallRegion,Material.withName("stone"))

		val endPos = VCR(5,0,0)
		val goal = new MoveGoal(VoxelRegion(endPos),MoveGait.Walk)
		creature.goals += goal

		advanceUntil(goal.complete, 30.seconds)

		assert(goal.complete)
		assert(creature.position == endPos)
		assert(!goal.path.exists(p => wallRegion.contains(p.position)))
	}
	
}
