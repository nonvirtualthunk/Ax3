package arx.eldr.game.logic.ai

/**
 * Test the AcquireEntityGoal in isolation (generally it's used as a subgoal of higher level goals)
 */

import arx.Prelude._
import arx.eldr.game.entity.data.InventoryData
import arx.eldr.game.entity.data.ItemData
import arx.eldr.game.entity.data.ItemFlag
import arx.eldr.game.entity.data.PhysicalData
import arx.eldr.game.logic.BaseTestEnv
import arx.eldr.game.logic.ai.goals.AcquireEntityGoal
import arx.eldr.game.logic.physics.PhysicalEntityGameComponent
import arx.eldr.game.requirements.ItemWithFlagRequirement
import arx.engine.entity.GameEntity
import org.scalatest.FlatSpec
import scalaxy.loops._

class AcquireEntityGoalTests extends FlatSpec {

	def testEnv() = new BaseTestEnv {
		val axe = new GameEntity("TestAxe")
		axe[PhysicalData]
		axe[ItemData].flags += ItemFlag.DurableEdge

		assert(PhysicalEntityGameComponent.storeEntityInInventory(axe, stockpile))

		engine.addComponent[AIGameComponent]
	}

	"A goal to acquire an item with a durable edge" should "locate the axe and acquire it" in {
		val env = testEnv()
		import env._
		
		val req = new ItemWithFlagRequirement(ItemFlag.DurableEdge, 1)
		val goal = new AcquireEntityGoal(req)
		creature.goals += goal

		advanceUntil(goal.complete, 10.seconds)

		assert(goal.complete)
		assert(goal.chosenEntities == Set(axe))
		assert(creature[InventoryData].heldEntities.contains(axe))
		assert(!stockpile[InventoryData].heldEntities.contains(axe))
		assert(creature.position.distanceTo(stockpile[PhysicalData].position) < 2.voxels)
	}

	"A goal to acquire two items with a durable edge" should "fail because there is only one axe" in {
		val env = testEnv()
		import env._

		val req = new ItemWithFlagRequirement(ItemFlag.DurableEdge, 2)
		val goal = new AcquireEntityGoal(req)
		creature.goals += goal

		advanceUntil(goal.interruptedBy.nonEmpty, 10.seconds)

		assert(!goal.complete)
		assert(goal.interruptedBy.nonEmpty)
	}

	"A goal to acquire two items with a durable edge when a second axe is added" should "located both axes and acquire them" in {
		val env = testEnv()
		import env._

		val req = new ItemWithFlagRequirement(ItemFlag.DurableEdge, 2)
		val goal = new AcquireEntityGoal(req)
		creature.goals += goal

		val secondAxe = new GameEntity("TestAxe-2")
		secondAxe[PhysicalData]
		secondAxe[ItemData].flags += ItemFlag.DurableEdge
		assert(PhysicalEntityGameComponent.storeEntityInInventory(secondAxe, stockpile))

		advanceUntil(goal.complete, 10.seconds)

		assert(goal.complete)
		assert(goal.chosenEntities == Set(axe, secondAxe))
		assert(creature[InventoryData].heldEntities.contains(axe))
		assert(creature[InventoryData].heldEntities.contains(secondAxe))
		assert(!stockpile[InventoryData].heldEntities.contains(axe))
		assert(!stockpile[InventoryData].heldEntities.contains(secondAxe))
	}
}
