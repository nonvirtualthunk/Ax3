package arx.eldr.game.logic.ai.goals

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.axistential.game.logic.ai.goals.MoveGait
import arx.core.units.UnitOfTime
import arx.eldr.game.entity.data.AIAgentData
import arx.eldr.game.entity.data.InventoryData
import arx.eldr.game.entity.data.PhysicalData
import arx.eldr.game.logic.ai._
import arx.eldr.game.logic.physics.PhysicalEntityGameComponent
import arx.eldr.game.requirements.Requirement
import arx.engine.entity.GameEntity
import arx.engine.entity.TGameEntity
import scalaxy.loops._

class AcquireEntityGoal(val requirement : Requirement) extends Goal {
	var chosenEntities : Set[TGameEntity] = Set()

	override def createPrerequisiteGoals(agent: TGameEntity): SubGoalResult = {
		if (requirement.amount == 0) {
			Nil
		} else {
			val allPossibleEntities = agent[AIAgentData].groups.flatMap(g => g[AIGroupData].entities)
			val allWithInventories = allPossibleEntities.filter(e => e.hasAuxData[InventoryData] && e.hasAuxData[PhysicalData])
			val allAppropriate = allWithInventories.filterNot(e => e[InventoryData].personalInventory).toList
			
			val mappedWithEntities = allAppropriate.map(e => 
				e -> e[InventoryData].heldEntities.filter(x => requirement.amountSatisfiedBy(x) > 0))
			
			val sortedContainers = mappedWithEntities.sortBy {
				case (container, entities) => container[PhysicalData].position.distanceTo(agent[PhysicalData].position).inMeters
			}
			
			
			var ret = List[Goal]()
			var amountSatisfied = 0
			for ((container,entities) <- sortedContainers if container != agent && amountSatisfied < requirement.amount) {
				var entitiesToTake = Set[TGameEntity]()
				for (ent <- entities if amountSatisfied < requirement.amount) {
					entitiesToTake += ent
					amountSatisfied += requirement.amountSatisfiedBy(ent)
					chosenEntities += ent
				}
				if (entitiesToTake.nonEmpty) {
					ret ::= new TakeEntitiesGoal(container, entitiesToTake)
				}
			}

			if (amountSatisfied >= requirement.amount) {
				ret
			} else {
				AIResult.Abort(AIReason.RequirementsNotMet(List(requirement),agent.world))
			}
		}
	}

	override def act(agent: TGameEntity, dt: UnitOfTime): AIResult = {
		AIResult.Success
	}

	override def progressRequired(agent: TGameEntity): Float = {
		0.0f
	}
}

class TakeEntitiesGoal(val takeFrom : TGameEntity, val entitiesToTake : Set[TGameEntity]) extends Goal {

	override def isValidAgent(agent: TGameEntity): Boolean = {
		agent.hasAuxData[PhysicalData] && agent.hasAuxData[InventoryData]
	}

	override def createPrerequisiteGoals(agent: TGameEntity): SubGoalResult = {
		val adjacentToTarget = takeFrom[PhysicalData].occupiedRegion.adjacentVoxels
		List(new MoveGoal(adjacentToTarget,MoveGait.Walk))
	}

	override def act(agentRaw: TGameEntity, dt: UnitOfTime): AIResult = {
		val agent = agentRaw.withData[PhysicalData].withData[InventoryData]
		val from = takeFrom.withData[PhysicalData].withData[InventoryData]
		
		// if the agent is adjacent to the target and the target still has the appropriate
		// entities
		val valid = from.occupiedRegion.adjacentVoxels.contains(agent.position) &&
						entitiesToTake.forall(e => from.heldEntities.contains(e))
		if (!valid) {
			AIResult.Abort(AIReason.InvalidState("Agent not in position to take entity, or entities missing from target", agent.world))
		} else {
			progress += dt.inSeconds
			if (progress >= progressRequired(agentRaw)) {
				entitiesToTake.foreach(e => PhysicalEntityGameComponent.storeEntityInInventory(e,agent))
				AIResult.Success
			} else {
				AIResult.Continue
			}
		}
	}

	override def progressRequired(agent: TGameEntity): Float = 1.0f
}