package arx.axistential.game.logic.ai.passivegoals

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/31/14
 * Time: 12:56 PM
 */

import arx.axistential.ai.AI.Reason.Unfinished
import arx.axistential.ai._
import arx.axistential.game.data.entity.AIData
import arx.axistential.game.entities.CreaturePropertiesData
import arx.axistential.game.logic.ai.AxisSearcher
import arx.axistential.game.logic.ai.goals.MoveGait
import arx.axistential.game.logic.ai.goals.MoveGoal
import arx.axistential.game.logic.general.MovementLogic
import arx.axistential.modules.hunger.ActivityLevel
import arx.core.units.UnitOfTime
import arx.tmp.game.logic.datastructures.voxelregions.VoxelRegion

class FleeFromEnemies extends PassiveGoal with PhysicalGoal with TNonLocationConstrainedGoal {
	override def activityLevel: ActivityLevel = ActivityLevel.Heavy

	override def priority ( agent : TAIAgent ) = {
		if ( agent.aux[AIData].detectedEnemies.nonEmpty ) {
			AI.Priority.AvoidingDeath
		} else {
			AI.Priority.Minimum
		}
	}

	def split(agent: TAIAgent): SplitResult = this

	def createPrerequisiteGoals(agent: TAIAgent) = {
		val enemies = agent.aux[AIData].detectedEnemies
		if ( enemies.isEmpty ) { Nil }
		else {
			val allViableDestinations = AxisSearcher.allPaths(
				AxistentialAllPathsQuery(
					agent,
					8,
					MovementLogic.moveCostFunction(agent),
					(_) => true,
					MovementLogic.obstructionFunction(agent.world),
					MovementLogic.isSupportedFunction(agent.world)
				)
			)

			if ( allViableDestinations.isEmpty ) { Nil }
			else {
				val supportedFunc = MovementLogic.isSupportedFunction(agent.world)

				val bestDest = allViableDestinations
					.filter(supportedFunc)
					.maxBy {
						v => enemies.map(e => e.position.distanceTo(v).inVoxels).min
					}

				val minDist = enemies.map(e => e.position.distanceTo(agent.position).inVoxels).min
				val maxObsDist = agent.aux[CreaturePropertiesData].maximumObservationRange
				val pcnt = minDist / maxObsDist.inVoxels

				val gait = 	if (pcnt < 0.85f) { MoveGait.Sprint }
								else if (pcnt < 0.95f) { MoveGait.Run }
								else { MoveGait.Jog }

				MoveGoal(VoxelRegion(bestDest),None, gait)
			}
		}
	}

	def act(agent: TAIAgent, dt : UnitOfTime): AIResult = {
		if ( agent.aux[AIData].detectedEnemies.isEmpty ) {
			AIResult.Success
		} else {
			AIResult.Retry( Unfinished )
		}
	}

	override def toString () = "FleeFromEnemies"
}
