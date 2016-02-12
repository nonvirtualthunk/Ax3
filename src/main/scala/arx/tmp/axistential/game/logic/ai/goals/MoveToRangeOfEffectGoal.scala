package arx.axistential.game.logic.ai.goals

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/17/13
 * Time: 12:50 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.general.MovementLogic
import arx.core.vec.coordinates.VoxelCoord

object MoveToRangeOfEffectGoal {
	// TODO: make this properly account for effector size as well as approaching from below
	def apply( effector : TPhysicalEntity, targetEntity : TPhysicalEntity ) : MoveGoal = this.apply(effector,targetEntity,MoveGait.Walk)
	def apply( effector : TPhysicalEntity, targetEntity : TPhysicalEntity , gait : MoveGait) : MoveGoal = {
		val region = MovementLogic.targetRegionFor(effector,targetEntity)
		MoveGoal(region,None,gait)
	}
	def apply( effector : TPhysicalEntity, targetVoxel : VoxelCoord) : MoveGoal = this.apply(effector,targetVoxel,MoveGait.Walk)
	def apply( effector : TPhysicalEntity, targetVoxel : VoxelCoord , gait : MoveGait) : MoveGoal = {
		val region = MovementLogic.targetRegionFor(effector,targetVoxel)
		MoveGoal(region,None,gait)
	}
}