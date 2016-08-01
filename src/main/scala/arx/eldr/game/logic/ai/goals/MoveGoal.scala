package arx.eldr.game.logic.ai.goals

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.axistential.game.logic.ai.goals.MoveGait
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.eldr.game.entity.data.PhysicalData
import arx.eldr.game.logic.ai.AIReason
import arx.eldr.game.logic.ai.AIResult
import arx.eldr.game.logic.ai.Goal
import arx.eldr.game.logic.ai.goals.MoveGoal.PathNode
import arx.eldr.game.logic.ai.search.PathQuery
import arx.eldr.game.logic.ai.search.Searcher
import arx.eldr.game.world.data.Terrain
import arx.eldr.graphics.data.EntityGraphicsData
import arx.engine.entity.TGameEntity

import scalaxy.loops._

class MoveGoal(destination: VoxelRegion, pace : MoveGait) extends Goal {
	var path = List[PathNode]()
	var currentIndex = 0
	var progressToNextStep = 0.0f

	override def plan(agent: TGameEntity): AIResult = {
		val PD = agent[PhysicalData]
		val start = PD.position

		if (destination.contains(start)) {
			path = Nil
			AIResult.Success
		} else {
			val TD = agent.world[Terrain]

			val query = new PathQuery(agent.world,start,destination)
			query.allowUnsupportedEndpoint = false
			query.climbLimit = 3
			query.jumpLimit = 1
			query.isSupportedFunction = (v) => TD.materialAt(v.x,v.y,v.z-1).solid
			query.obstructionFunction = (v) => TD.materialAt(v.x,v.y,v.z).solid
			query.moveCostFunction = (v1,v2,c,j) => if (c == 0 && j == 0) { distance(v1,v2) } else { 3 }

			Searcher.pathTo(query) match {
				case Some(pathList) =>
					path = pathList.map(v => PathNode(v))
					currentIndex = 0
					progressToNextStep = 0.0f
					AIResult.Success
				case None =>
					AIResult.Abort(AIReason.NoPathFound(start, destination, agent.world))
			}
		}
	}

	override def act(agent: TGameEntity, dt: UnitOfTime): AIResult = {
		if (path.size <= 1) {
			return AIResult.Success
		}

		val stepDuration = 0.4f
		if (currentIndex + 1 < path.size) {
			val prevStep = path(currentIndex).position.toObjectCoordFoot
			val nextStep = path(currentIndex+1).position.toObjectCoordFoot

			val curPos = prevStep + (nextStep - prevStep) * (progressToNextStep / stepDuration)
			agent[EntityGraphicsData].graphicalPosition = curPos
			agent[EntityGraphicsData].facing = (nextStep - prevStep).normalizeSafe
		}

		progressToNextStep += dt.inSeconds
		while (progressToNextStep > stepDuration) {
			currentIndex += 1
			agent[PhysicalData].position = path(currentIndex).position
			progressToNextStep -= stepDuration
		}

		if (currentIndex >= path.size - 1) {
			AIResult.Success
		} else {
			AIResult.Continue
		}
	}

	override def progressRequired(agent: TGameEntity): Float = {
		path.size
	}

	override def onGoalEnded(agent : TGameEntity): Unit = {
		agent[EntityGraphicsData].graphicalPosition = ObjectCoord.Sentinel
	}
}

object MoveGoal {
	case class PathNode(position : VoxelCoord)
}