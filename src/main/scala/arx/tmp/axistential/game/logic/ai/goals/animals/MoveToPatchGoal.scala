package arx.axistential.game.logic.ai.goals.animals

import arx.application.Noto
import arx.axistential.ai.AI.Reason.NoPath
import arx.axistential.ai.AILogging
import arx.axistential.ai.AIReason
import arx.axistential.ai.AIResult
import arx.axistential.ai.TAIAgent
import arx.axistential.game.data.entity.AnimalAIData.Patch
import arx.axistential.game.logic.ai.goals.MoveGoal
import arx.axistential.game.world.AxistentialWorld.AxistentialWorld
import arx.core.units.UnitOfTime
import arx.tmp.game.logic.datastructures.voxelregions.VoxelRegion

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/30/15
 * Time: 5:43 PM
 */

class MoveToPatchGoal (fromPatch : Option[Patch], toPatch : Patch) extends MoveGoal(VoxelRegion(toPatch.region.center,7)) {
	override def onPlanningAborted(agent: TAIAgent, reason : AIReason): Unit = {
		reason match {
			case NoPath(_,_,_) => fromPatch match {
				case Some(from) =>
					Noto.finest(AILogging,s"No path available from $from to $toPatch, MoveToPatchGoal aborting")
					from.pathCostTo += toPatch -> 100000.0f
				case _ =>
			}
			case _ =>
		}
	}

	override def act(agent: TAIAgent, dt: UnitOfTime): AIResult = {
		super.act (agent, dt) match {
			case AIResult.Success =>
				toPatch.lastArrivedAt = agent.world.time
				AIResult.Success
			case other => other
		}
	}
}
