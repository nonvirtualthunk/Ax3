package arx.axistential.game.logic.ai.goals.animals

import arx.Prelude._
import arx.application.Noto
import arx.axistential.ai._
import arx.axistential.ai.traits.TRecalculatingGoal
import arx.axistential.game.components.physics.DebugData
import arx.axistential.game.data.entity.AIData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.entities.CreatureEntity
import arx.axistential.game.entities.CreatureStateData
import arx.axistential.game.logic.ai.AxisSearcher
import arx.axistential.game.logic.ai.goals.MoveGait
import arx.axistential.game.logic.ai.goals.MoveGoal
import arx.axistential.game.logic.general.MovementLogic
import arx.axistential.game.logic.general.PhysicalEntityLogic
import arx.axistential.modules.hunger.ActivityLevel
import arx.core.units.UnitOfDistance
import arx.core.units.UnitOfTime
import arx.core.vec.Vec4f
import arx.core.vec.coordinates.ObjectCoord
import arx.tmp.game.logic.datastructures.voxelregions.VoxelRegion

/**
 * totalDistanceAllowed - how far in total we will travel before abandoning this chase
 * maxRangeToTarget - how far away from the target we can get before we abandon this chase
 */
class ChaseGoal (
	 target : TPhysicalEntity,
	 totalDistanceAllowed: UnitOfDistance,
	 maxRangeToTarget : UnitOfDistance,
	 sneak : Boolean,
	 maxGait : MoveGait
) extends PhysicalGoal with TRecalculatingGoal {

	var totalDistanceTravelled = 0.meters
	var plannedFrom = ObjectCoord.Sentinel

	/** Should return true if a recalculation is needed at this time, false otherwise */
	override def needsRecalculation(agent : TAIAgent) : Boolean = {
		if (agent.adjustedFootPos.scalarDistanceTo(plannedFrom) > 2.25f) {
			Noto.finest(AILogging,"[recalc] triggered in chase goal")
			true
		} else {
			false
		}
	}

	/** Should return a top level copy of this goal. Should not copy internals, planned
	  * information, progress, etc. It should essentially supply matching constructor args
	  * and nothing else. */
	override def createTopLevelCopy: ChaseGoal = new ChaseGoal(target,totalDistanceAllowed,maxRangeToTarget,sneak,maxGait)

	override def activityLevel: ActivityLevel = ActivityLevel.Heavy

	override def progressRequired(agent: TAIAgent): Float = 0.0f

	/** If this goal can be split into more than one part, create two new goals, the first representing
	  * the split off portion, the second representing the remainder
	  * @param agent the agent who is to perform the split off section
	  */
	override def split(agent: TAIAgent): SplitResult = this

	override def createPrerequisiteGoals(agent: TAIAgent): PrerequisiteGoalsResult = {
		val targetRegion = MovementLogic.targetRegionFor(agent,target)
		plannedFrom = agent.adjustedFootPos

		val obstructionFunc = MovementLogic.obstructionFunction(agent.world)

			AxisSearcher.pathTo(AxistentialPathQuery(
				agent : TPhysicalEntity,
				targetRegion,
				totalDistanceAllowed.inVoxels.toInt,
				MovementLogic.moveCostFunction(agent),
				(_) => true,
				obstructionFunc,
				MovementLogic.isSupportedFunction(agent.world)
			)) match {
				case Some(path) =>
					val pathPart = path
					pathPart match {
						case Nil => Nil
						case _ =>
							// If we haven't been spotted and we're supposed to be sneaking, move slowly, otherwise, go with max gait
							val detected = agent match {
								case ce : CreatureEntity if target.aux[AIData].detectedEnemies.contains(ce) => true
								case _ => false
							}
							val gait = if (!detected && sneak) { MoveGait.Sneak }
											else { maxGait }

							println(s"Chase continuing, detected : $detected, sneaking : $sneak, gait: $gait")
							agent.aux[CreatureStateData].stealthActive = ! detected && sneak
							MoveGoal(VoxelRegion(pathPart.last),Some(pathPart),gait)
					}
				case None =>
					val DD = agent.world.aux[DebugData]

					val possibleTargets = targetRegion.toSet
						.filter(MovementLogic.isSupportedFunction(agent.world))
						.filterNot(MovementLogic.obstructionFunction(agent.world))

					DD.active = true
					DD.voxelGroups += "ChaseDebug" -> DebugData.VoxelGroup(Vec4f(0.2f,0.2f,0.8f,0.3f),possibleTargets)

	//				val TD = agent.world.terrainData
	//				val allTaleaRevisions = (agent.footVoxelPosition :: targetRegion.toList)
	//					.map(v => VoxelCoord((v >> Talea.dimensionPo2) << Talea.dimensionPo2))
	//					.distinct
	//					.map(v => v -> TD.materialGrid.taleaFor(v.x,v.y,v.z).modifiedCount)
	//					.toMap
	//				AI.Reason.NoPath(agent.footVoxelPosition,targetRegion,allTaleaRevisions)
					AI.Reason.NoPathToMovingTarget(agent.adjustedFootVoxelPos,target)
			}
	}

	override def fitness(agent: TAIAgent): Int = AI.Fitness.Normal

	override def act(agent: TAIAgent, dt: UnitOfTime): AIResult = {
		val dist = PhysicalEntityLogic.minimumDistanceBetweenEntities(agent,target)
		if (dist <= 1.5.voxel) {
			Success
		} else if (dist > maxRangeToTarget) {
			Fail(AI.Reason.OutOfRange(agent,target,maxRangeToTarget))
		} else if (totalDistanceTravelled > totalDistanceAllowed) {
			Fail(AI.Reason.NoValidTarget(zeroSeconds))
		} else {
			Retry(AI.Reason.Unfinished)
		}
	}

	override def onReset() {
		totalDistanceTravelled = 0.meters
	}
}
