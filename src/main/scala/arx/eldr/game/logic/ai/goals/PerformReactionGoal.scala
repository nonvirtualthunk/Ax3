package arx.eldr.game.logic.ai.goals

/**
 * TODO: Add javadoc
 */

import arx.axistential.game.logic.ai.goals.MoveGait
import arx.core.datastructures.MultiMap
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.units.UnitOfTime
import arx.eldr.game.archetypes.InputKind
import arx.eldr.game.archetypes.OutputKind
import arx.eldr.game.archetypes.Reaction
import arx.eldr.game.entity.data.InventoryData
import arx.eldr.game.entity.data.PhysicalData
import arx.eldr.game.logic.ai.AIReason
import arx.eldr.game.logic.ai.AIResult
import arx.eldr.game.logic.ai.Goal
import arx.eldr.game.logic.ai.SubGoalResult
import arx.engine.entity.TGameEntity
import arx.engine.requirement.Requirement
import arx.engine.requirement.SpecificEntityDescriptor

class PerformReactionGoal(val reaction: Reaction,
								  val specifiedInputs: MultiMap[InputKind, TGameEntity],
								  val specifiedTools: MultiMap[InputKind, TGameEntity],
								  val specifiedLocation: Option[VoxelRegion]) extends Goal {

	var acquireGoalsByIdent = new MultiMap[InputKind, AcquireEntityGoal]
	var createdEntities = new MultiMap[OutputKind, TGameEntity]

	override def isValidAgent(agent: TGameEntity): Boolean = {
		reaction.actors.forall(r => r.isSatisfiedBy(agent))
	}

	override def createPrerequisiteGoals(agent: TGameEntity): SubGoalResult = {
		var gatherPrereqs = List[Goal]()
		var movePrereqs: Option[Goal] = None

		val specifiedEntities = new MultiMap[InputKind, TGameEntity]
		specifiedEntities.addAll(specifiedInputs)
		specifiedEntities.addAll(specifiedTools)

		var requirements = Map[InputKind, Requirement]()
		requirements ++= reaction.inputs
		requirements ++= reaction.tools

		for ((ident, req) <- requirements) {
			val specified = specifiedEntities.get(ident)
			val amountSpecified = specified.map(req.amountSatisfiedBy).sum

			ident match {
				case InputKind.Target(_) =>
					if (amountSpecified < req.amount || specified.size > 1) {
						throw new IllegalStateException("unspecified target input...cannot be inferred " + ident)
					} else {
						val moveTo = specified.head
						if (moveTo.hasAuxData[PhysicalData]) {
							movePrereqs = Some(new MoveGoal(moveTo.aux[PhysicalData].occupiedRegion.adjacentVoxels, MoveGait.Walk))
						}
					}
				case _ =>
					var newGoals = List[AcquireEntityGoal]()

					val effectiveReq = if (amountSpecified > 0) {
						for (tool <- specified) {
							newGoals ::= new AcquireEntityGoal(SpecificEntityDescriptor(tool))
						}
						val remainder = req.amount - amountSpecified
						req.copyWithAmount(remainder)
					} else {
						req
					}

					newGoals ::= new AcquireEntityGoal(effectiveReq)
					acquireGoalsByIdent.addAll(ident, newGoals)
					gatherPrereqs = gatherPrereqs ::: newGoals
			}
		}

		gatherPrereqs ::: movePrereqs.toList
	}

	override def act(agent: TGameEntity, dt: UnitOfTime): AIResult = {
		// The region of effect should be everything in the same region or adjacent to
		// the acting entity
		var regionOfEffect = agent[PhysicalData].occupiedRegion
		regionOfEffect = regionOfEffect.union(regionOfEffect.adjacentVoxels)

		for ((ident, goals) <- acquireGoalsByIdent.intern; goal <- goals) {
			for (ent <- goal.chosenEntities) {
				if (ent.hasAuxData[PhysicalData]) {
					if (!regionOfEffect.contains(ent[PhysicalData].position)) {
						return AIResult.Retry(AIReason.InvalidState("Missing entity: " + ent, agent.world))
					}
				}
			}
		}

		progress += dt.inSeconds
		if (progress > progressRequired(agent)) {
			performReaction(agent)
			AIResult.Success
		} else {
			AIResult.Continue
		}
	}

	def performReaction(agent: TGameEntity): Unit = {
		val world = agent.world

		val allInputs = new MultiMap[InputKind, TGameEntity]
		for (inputKey <- reaction.inputs.keys) {
			val allEntities = acquireGoalsByIdent.get(inputKey).flatMap(_.chosenEntities) ++
				specifiedInputs.get(inputKey).toSet
			allInputs.addAll(inputKey, allEntities)
		}

		val allTools = new MultiMap[InputKind, TGameEntity]
		for (toolKey <- reaction.tools.keys) {
			val allEntities = acquireGoalsByIdent.get(toolKey).flatMap(_.chosenEntities) ++
				specifiedInputs.get(toolKey).toSet
			allInputs.addAll(toolKey, allEntities)
		}

		createdEntities = reaction.react(allInputs, allTools, agent[PhysicalData].position, List(agent))

		for (input <- allInputs.flattenedValues) {
			world.removeEntity(input)
		}
		for (result <- createdEntities.flattenedValues) {
			if (result.hasAuxData[PhysicalData]) {
				result[PhysicalData].position = agent[PhysicalData].position
				result[PhysicalData].heldBy = Some(agent)
				agent[InventoryData].heldEntities += result
			}
			world.addEntity(result)
		}
	}

	override def progressRequired(agent: TGameEntity): Float = {
		reaction.baseTimeCost.inSeconds
	}
}
