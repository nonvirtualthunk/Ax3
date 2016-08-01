package arx.eldr.game.logic.ai

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.core.datastructures.MultiMap
import arx.core.vec.Vec3i
import arx.eldr.game.archetypes._
import arx.eldr.game.archetypes.plants._
import arx.eldr.game.entity.data.InventoryData
import arx.eldr.game.entity.data.ItemData
import arx.eldr.game.entity.data.ItemFlag
import arx.eldr.game.entity.data.PhysicalData
import arx.eldr.game.logic.BaseTestEnv
import arx.eldr.game.logic.ai.goals.PerformReactionGoal
import arx.eldr.game.requirements.ItemWithFlagDescriptor
import arx.engine.entity.GameEntity
import arx.engine.entity.TGameEntity
import org.scalatest.FlatSpec

import scalaxy.loops._

class PerformReactionGoalTests extends FlatSpec {

	def testEnv() = new BaseTestEnv {
		engine.addComponent[AIGameComponent]

		val oakSpecies = PlantSpecies.OakTree
		val oak = oakSpecies.createPlant().withData[PlantData]
		oak.age = oak.ageCategoryStarts(AgeCategory.Tree.Sapling) + 1.second
		oak[PhysicalData].position = VCR(5,0,0)

		world.addEntity(oak)

		creature[InventoryData].heldEntities = Set()
	}

	"A gather sapling goal" should "result in wood being gathered and sapling removed" in {
		val env = testEnv()
		import env._

		val reaction = Reaction.reactionsByName(s"gather ${oakSpecies.name} sapling")
		val specifiedInputs = MultiMap(reaction.inputs.keys.head -> (oak : TGameEntity))
		val goal = new PerformReactionGoal(reaction,specifiedInputs,MultiMap.empty,None)
		creature.goals += goal

		engine.updateSerial(0.0166667f,1000)

		assert(goal.complete)
		assert(goal.acquireGoalsByIdent.intern.isEmpty) // nothing to acquire for this goal
		assert(goal.createdEntities.flattenedValues.size == 1)
		assert(goal.createdEntities.flattenedValues.head.hasAuxData[MaterialBlockData])
		assert(creature[InventoryData].heldEntities.contains(goal.createdEntities.flattenedValues.head))
		assert(creature.position == VCR(4,0,0))

		assert(oak.world.isSentinel)
	}

	"A chop tree goal" should "fail if there is no appropriate tool to be acquired" in {
		val env = testEnv()
		import env._

		oak.age = oak.ageCategoryStarts(AgeCategory.Tree.Mature) + 1.second
		val reaction = ChopTreeReaction(oak)
		val specifiedInputs = MultiMap(reaction.inputs.keys.head -> (oak : TGameEntity))

		val goal = new PerformReactionGoal(reaction,specifiedInputs,MultiMap.empty,None)
		creature.goals += goal

		engine.updateSerial(0.0166667f,600)

		assert(goal.interruptedBy.nonEmpty)
		assert(!goal.complete)
		assert(oak.world.notSentinel)
	}

	"A chop tree goal" should "result in an axe being acquired and a tree cut down into wood" in {
		val env = testEnv()
		import env._

		oak.age = oak.ageCategoryStarts(AgeCategory.Tree.Mature) + 1.second
		val reaction = ChopTreeReaction(oak)
		val specifiedInputs = MultiMap(reaction.inputs.keys.head -> (oak : TGameEntity))

		val axe = new GameEntity("Test-Axe")
		axe[ItemData].flags += ItemFlag.DurableEdge
		axe[PhysicalData].position = VCR(0,5,0)
		axe[PhysicalData].heldBy = Some(stockpile)
		stockpile[InventoryData].heldEntities += axe

		world.addEntity(stockpile)
		world.addEntity(axe)

		val goal = new PerformReactionGoal(reaction,specifiedInputs,MultiMap.empty,None)
		creature.goals += goal

		engine.updateSerial(0.0166667f,600)

		assert(goal.complete)
		assert(goal.acquireGoalsByIdent.intern.nonEmpty) // nothing to acquire for this goal
		assert(goal.createdEntities.flattenedValues.size == oak[TreeData].sticks.toInt + oak[TreeData].trunkSize.toInt)
		assert(goal.createdEntities.flattenedValues.head.hasAuxData[MaterialBlockData])
		assert(creature[InventoryData].heldEntities.contains(goal.createdEntities.flattenedValues.head))
		assert(creature[InventoryData].heldEntities.contains(axe))

		assert(oak.world.isSentinel)
	}

	"A chop tree goal on a tree with additional requirements" should "result in the proper items being acquired and the tree cut down" in {
		val env = testEnv()
		import env._

		oak.age = oak.ageCategoryStarts(AgeCategory.Tree.Mature) + 1.second
		oak[TreeData].choppingToolRequirements ::= ItemWithFlagDescriptor(ItemFlag.SharpEdge)
		val reaction = ChopTreeReaction(oak)
		val specifiedInputs = MultiMap(reaction.inputs.keys.head -> (oak : TGameEntity))

		val axe = new GameEntity("Test-Axe")
		axe[ItemData].flags += ItemFlag.DurableEdge
		axe[PhysicalData].position = VCR(0,5,0)
		axe[PhysicalData].heldBy = Some(stockpile)
		stockpile[InventoryData].heldEntities += axe

		val superAxe = new GameEntity("Super-Axe")
		superAxe[ItemData].flags += ItemFlag.DurableEdge
		superAxe[ItemData].flags += ItemFlag.SharpEdge
		superAxe[PhysicalData].position = VCR(0,5,0)
		superAxe[PhysicalData].heldBy = Some(stockpile)
		stockpile[InventoryData].heldEntities += superAxe

		world.addEntities(stockpile, axe, superAxe)

		val goal = new PerformReactionGoal(reaction,specifiedInputs,MultiMap.empty,None)
		creature.goals += goal

		engine.updateSerial(0.0166667f,600)

		assert(goal.complete)
		assert(goal.acquireGoalsByIdent.intern.nonEmpty) // nothing to acquire for this goal
		assert(goal.createdEntities.flattenedValues.size == oak[TreeData].sticks.toInt + oak[TreeData].trunkSize.toInt)
		assert(goal.createdEntities.flattenedValues.head.hasAuxData[MaterialBlockData])
		assert(creature[InventoryData].heldEntities.contains(goal.createdEntities.flattenedValues.head))
		assert(creature[InventoryData].heldEntities.contains(superAxe))

		assert(oak.world.isSentinel)
	}
}
