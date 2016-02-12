package arx.axistential.game.logic.ai.passivegoals

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/28/14
 * Time: 8:50 AM
 */

import arx.axistential.ai.AI.Reason.InvalidAgent
import arx.axistential.ai.AI.Reason.UnexpectedState
import arx.axistential.ai._
import arx.axistential.game.data.world.DesignationData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.ai.AxisSearcher
import arx.axistential.game.logic.ai.goals.MoveGoal
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec3i
import arx.tmp.game.logic.datastructures.voxelregions.VoxelRegion

class MoveOutOfBadLocationGoal extends Goal with PassiveGoal {
	
	def badSpot (agent : TAIAgent, v : VoxelCoord, dims : ReadVec3i) : Boolean = {
		val DD = agent.world.aux[DesignationData]
		val fp = v
		for (z <- -1 until dims.z) {
			if (DD.anyDesignationAt(fp.x,fp.y,fp.z + z)) {
				return true
			}
		}
		false
	}
	
	override def priority (agent : TAIAgent) : Int = {
		agent match {
			case pe : TPhysicalEntity => {
				if (badSpot(agent,pe.adjustedFootVoxelPos,Vec3i(pe.boundingDimensions.inVoxels))) {
					AI.Priority.Low
				} else {
					AI.Priority.Minimum
				}
			}
			case _ => AI.Priority.Minimum
		}
	}

	def isSupported(agent:TAIAgent) = {
		val terrain = agent.world.aux[TerrainData]
		(v:VoxelCoord) => terrain.isSolid(v.minusZ(1))
	}
	def isObstructed(agent:TAIAgent) = {
		val terrain = agent.world.aux[TerrainData]
		(v:VoxelCoord) => terrain.isSolid(v)
	}

	override def createPrerequisiteGoals(agent: TAIAgent): PrerequisiteGoalsResult = {
		agent match {
			case pe : TPhysicalEntity =>
				val allPaths = AxisSearcher.allPaths(AxistentialAllPathsQuery(
					pe,
					10,
					(v1,v2,c,j) => MathPrelude.distance(v1,v2),
					(v:VoxelCoord) => true,
					isObstructed(agent),
					isSupported(agent)
				))

				val dim = Vec3i(pe.boundingDimensions.inVoxels)
				allPaths.find(v => !badSpot(agent,v,dim)) match {
					case Some(good) => {
						PrerequisiteGoalsResult.Success(MoveGoal(VoxelRegion(good)) :: Nil)
					}
					case None => PrerequisiteGoalsResult.Failure(this,UnexpectedState("Could not find good location to move to"))
				}
			case _ => PrerequisiteGoalsResult.Failure(this,InvalidAgent(agent))
		}
	}

	/** If this goal can be split into more than one part, create two new goals, the first representing
	  * the split off portion, the second representing the remainder
	  * @param agent the agent who is to perform the split off section
	  */
	override def split(agent: TAIAgent): SplitResult = this
	override def act(agent: TAIAgent, dt: UnitOfTime): AIResult = Success
}
