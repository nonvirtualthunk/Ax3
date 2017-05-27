package arx.eldr.game.logic.ai

/**
 * TODO: Add javadoc
 */

import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.traits.ArxEnum
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.entity.TGameEntity
import arx.engine.world.World
import arx.Prelude._
import arx.engine.requirement.Requirement

import scala.language.implicitConversions

abstract class Goal {
	var assignedTo: Option[TGameEntity] = None
	var priority: GoalPriority = GoalPriority.Medium
	var claimedEntities: Set[TGameEntity] = Set()

	var prerequisiteGoals: List[Goal] = Nil
	var followOnGoals: List[Goal] = Nil
	var interruptedBy: List[AIReason] = Nil

	var planned = false
	var prerequisitesCreated = false
	var complete = false
	var fromGroup: Option[TGameEntity] = None
	var progress = 0.0f


	def plan(agent: TGameEntity): AIResult = AIResult.Success

	def createPrerequisiteGoals(agent: TGameEntity): SubGoalResult = SubGoalResult(AIResult.Success, Nil)

	def createFollowOnGoals(agent: TGameEntity): SubGoalResult = SubGoalResult(AIResult.Success, Nil)

	def act(agent: TGameEntity, dt: UnitOfTime): AIResult

	def progressRequired(agent: TGameEntity): Float

	def split(agent: TGameEntity): SplitResult = SplitResult.Single(this)

	def isValidAgent(agent : TGameEntity) : Boolean = true

	def onGoalEnded(agent : TGameEntity) : Unit = {}

	def leafGoal : Goal = prerequisiteGoals.filterNot(_.complete) match {
		case unfinishedPrerequisite :: _ => unfinishedPrerequisite.leafGoal
		case Nil => this
	}

	override def toString: String = this.getClass.getSimpleName
}

sealed abstract class SplitResult

object SplitResult {

	case class Split(splitOff: Goal, remainder: Goal) extends SplitResult

	implicit class Single(val goal: Goal) extends SplitResult

	implicit class None(val reason: AIReason) extends SplitResult

	implicit def tuple2split(tup: (Goal, Goal)): SplitResult = Split(tup._1, tup._2)

	//	implicit def single2split ( single : Goal ) : SplitResult = Single(single)
}

class SubGoalResult(val result: AIResult, val goals: List[Goal])

object SubGoalResult {
	def apply(result: AIResult, goals: List[Goal]) = new SubGoalResult(result, goals)

	implicit def goalsToResult(goals: List[Goal]): SubGoalResult = new SubGoalResult(AIResult.Success, goals)

	implicit def failureToResult(result: AIResult): SubGoalResult = new SubGoalResult(result, Nil)
}

class GoalPriority(nomen: String, val value: Int) extends ArxEnum(nomen)

object GoalPriority {
	val VeryLow = new GoalPriority("Very Low", -100)
	val Low = new GoalPriority("Low", -50)
	val Medium = new GoalPriority("Medium", 0)
	val High = new GoalPriority("High", 50)
	val VeryHigh = new GoalPriority("Very High", 100)
}


abstract class AIReason(val world: World) {
	var timestamp = world.time

	def stillApplies(agent: TGameEntity): Boolean
}

object AIReason {

	case class RequirementsNotMet(requirements: List[Requirement], w: World) extends AIReason(w) {
		override def stillApplies(agent: TGameEntity): Boolean = true
	}

	case class NoPathFound(from : VoxelCoord, to : VoxelRegion, w : World) extends AIReason(w) {
		override def stillApplies(agent: TGameEntity): Boolean = {
			agent.world.time - timestamp < 20.seconds
		}
	}

	case class InvalidState(message : String, w : World) extends AIReason(w) {
		override def stillApplies(agent: TGameEntity): Boolean = false
	}

}


class AIResult

object AIResult {

	case class Abort(reason: AIReason) extends AIResult

	case class Retry(reason: AIReason) extends AIResult

	case class Fail(reason: AIReason) extends AIResult

	case object Continue extends AIResult

	case object Success extends AIResult

}
