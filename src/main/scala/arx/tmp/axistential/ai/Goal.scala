package arx.axistential.ai

/**
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.ai.AI.Reason.IsSentinelReason
import arx.axistential.game.entities.traits.PhysicalEntityData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.modules.hunger.ActivityLevel
import arx.core.Moddable
import arx.core.datastructures.OneOrMore
import arx.core.traits.TSentinel
import arx.core.traits.TSentinelable
import arx.core.units.UnitOfTime
import arx.engine.game.GameEngine
import arx.engine.world.World
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.core.TSanityCheckable

import scala.language.implicitConversions

trait Goal extends Serializable with TSentinelable with TSanityCheckable {
	var interruptedBy = List[AIReason]()
	protected[ai] var _finished = false
	protected[ai] var _active = true
	protected[ai] var _removed = false
	/** If this goal was split off, this will point at the other half of the result of the split operation */
	protected[ai] var _splitFrom : Option[Goal] = None
	protected[ai] var _splitInto : Option[OneOrMore[Goal]] = None
	protected[ai] var _fromGroup = false
	/** If true, deltaTime passed to act(...) will not be modified by the agent's action rate, otherwise it will be */
	protected[ai] var usesRawTime = false

	var _priority = Moddable[Int](0) // Normal priority, range [-100,100]
	var subGoals = List[Goal]()
	var waitingOn = List[Goal]()
	var parentGoal : Option[Goal] = None
	var progress = 0.0f
	var claimedEntities = List[GameEntity]()

	def priority (agent : TAIAgent) = _priority.resolve()
	/** Filters out goals that are waiting on some other precondition in order to advance and plan */
	def isReady (agent : TAIAgent) : Boolean = true
	def markFinished () { _finished = true }
	def finished : Boolean = (_finished && subGoals.forall(_.finished)) || (_splitInto.nonEmpty && _splitInto.get.forall(_.finished))
	def markActive () { _active = true }
	def markInactive () { _active = false }
	def isActive(world: World) = _active
	def isInterrupted(world: World, agent: TAIAgent) = {
		interruptedBy.exists ( i => interruptionApplies(i, world, agent) && i.stillApplies(world, agent) )
	}
	def selfAndAllSubGoals : List[Goal] = this :: subGoals.flatMap( _.selfAndAllSubGoals )
	def selfAndAllProgenitors : List[Goal] = this :: (parentGoal match {
		case Some(pg) => pg.selfAndAllProgenitors
		case None => Nil
	})
	def topLevelParent : Goal = parentGoal match { case Some(pg) => pg.topLevelParent ; case None => this }
	
	protected def interruptionApplies(interruption: AIReason, world: World, agent: TAIAgent) = true

	// Convenience methods for the various results, could also be modified in future to automatically embed meta-data
	// or at least identity information as to where the success/failure came from
	protected def Abort (reason : AIReason) = AIResult.Abort(reason)
	protected def Retry (reason : AIReason) = AIResult.Retry(reason)
	protected def Fail (reason : AIReason) = AIResult.Fail(reason)
	protected def Success = AIResult.Success
	protected def Continue = AIResult.Continue

	/**
	 * Plan this goal, all its subgoals, all of their subgoals, and so on. Note that this can cause slightly odd behavior
	 * in multipart goals. For example, when planning AcquireItems there are subgoals to move to the item source and then
	 * to the item destination. When this is called at the beginning, both will be planned from the agent's current location.
	 * When time comes to execute the move to destination, the agent will then be off path, having moved to the source, and
	 * the move goal will need to be re-planned. We could potentially eliminate this, but there are advantages to leaving
	 * it as is, in the above case for example it catches situations in which the destination is unreachable before doing
	 * any work.
	 */
	def planRecursive(agent:TAIAgent) : PlanningResult = {
		plan(agent) match {
			case AIResult.Success => {
				for ( g <- subGoals ) {
					val subRet = g.planRecursive(agent)
					if ( subRet.result != AIResult.Success ) { return subRet }
				}
				PlanningResult(AIResult.Success,this)
			}
			case other => PlanningResult(other,this)
		}
	}

	final def reset () {
		progress = 0.0f
		onReset()
	}
	final def failed (agent:TAIAgent, reason : AIReason) {
		onFail(agent,reason)
	}
	final def aborted (agent:TAIAgent, reason : AIReason) {
		onAbort(agent,reason)
	}
	final def planningFailed (agent:TAIAgent, reason : AIReason): Unit = {
		onPlanningFailed(agent,reason)
	}
	final def planningAborted (agent:TAIAgent, reason : AIReason): Unit = {
		onPlanningAborted(agent,reason)
	}

	def createPrerequisiteGoals(agent : TAIAgent) : PrerequisiteGoalsResult
	def fitness ( agent : TAIAgent ) : Int // [-100,100] with 0 being no opinion either way
	/** If this goal can be split into more than one part, create two new goals, the first representing
	  * the split off portion, the second representing the remainder
 *
	  * @param agent the agent who is to perform the split off section
	  */
	def split ( agent : TAIAgent ) : SplitResult
	def allSplits ( agent : TAIAgent ) : List[Goal] = {
		this.split(agent) match {
			case n : SplitResult.None => Nil
			case s : SplitResult.Single => List(s.goal)
			case SplitResult.Split(a,b) => a :: b.allSplits(agent)
		}
	}

	def plan ( agent : TAIAgent ) : AIResult = AIResult.Success
	def act ( agent : TAIAgent , dt : UnitOfTime) : AIResult
	def progressRequired ( agent : TAIAgent ) : Float
	def onReset() {}
	def onAbort( agent : TAIAgent, reason : AIReason ) {}
	def onFail( agent : TAIAgent, reason : AIReason ) {}
	def onPlanningFailed(agent : TAIAgent,reason : AIReason) {}
	def onPlanningAborted(agent : TAIAgent,reason : AIReason) {}
	def onAdded( group : TAIGroup ) {}

	def andThen (otherGoal : Goal) = {
		otherGoal.waitingOn ::= this
		SplitResult.Split(this,otherGoal)
	}

	protected[ai] def isValidAgent (agent : TAIAgent) : Boolean = true

//	def joinWith ( other : Goal ) : Goal = this.join(other.asInstanceOf[this.type])
//	protected def join ( other : this.type ) : Goal

	protected implicit def goalToPrereqSuccess (goals : Goal) : PrerequisiteGoalsResult = new PrerequisiteGoalsResult.Success(List(goals))
	protected implicit def goalListToPrereqSuccess (goals : List[Goal]) : PrerequisiteGoalsResult = new PrerequisiteGoalsResult.Success(goals)
	protected implicit def reasonToPrereqFailure(reason:AIReason) : PrerequisiteGoalsResult = new PrerequisiteGoalsResult.Failure(this,reason)

}

class AlternativesGoal(possibleGoals : List[Goal]) extends Goal {
	override def createPrerequisiteGoals(agent: TAIAgent): PrerequisiteGoalsResult = {
		// This doesn't really work, because any interruptions will be carried all the way up to the top level goal
		// which won't be retried until they no longer apply, so....
		val filteredGoals = possibleGoals.filter(g => ! g.interruptedBy.exists(i => i.stillApplies(agent.world, agent)))
		val sortedGoals = filteredGoals.sortBy(g => g.fitness(agent))
		PrerequisiteGoalsResult.Possibilities(sortedGoals.map(g => g.createPrerequisiteGoals(agent)))
	}

	override def progressRequired(agent: TAIAgent): Float = 0.0f
	override def split(agent: TAIAgent): SplitResult = this
	override def fitness(agent: TAIAgent): Int = 0
	override def act(agent: TAIAgent, dt: UnitOfTime): AIResult = Success
}

trait PhysicalGoal extends Goal {
	def activityLevel : ActivityLevel

	override protected[ai] def isValidAgent (agent : TAIAgent) : Boolean = agent.isInstanceOf[TPhysicalEntity]

	protected implicit def agentToPhysicalEntity (agent:TAIAgent) : TPhysicalEntity = agent.as[TPhysicalEntity]
	protected implicit def agentToPhysicalData (agent:TAIAgent) : PhysicalEntityData = agent.as[TPhysicalEntity].physData
}

case class PlanningResult (result : AIResult, fromGoal : Goal)

object Goal {
	val Sentinel : Goal = new Goal with TSentinel {
		override def createPrerequisiteGoals(agent: TAIAgent): PrerequisiteGoalsResult = new PrerequisiteGoalsResult.Success(Nil)
		override def progressRequired(agent: TAIAgent): Float = 0.0f
		override def split(agent: TAIAgent): SplitResult = SplitResult.None(IsSentinelReason)
		override def fitness(agent: TAIAgent): Int = 0
		override def act(agent: TAIAgent, dt: UnitOfTime): AIResult = AIResult.Success
	}
}

/**
 * A Goal that is primarily a container for other, simpler goals. A goal to launch
 * an airship expedition, for example. Its prerequisites are high level goals themselves.
 */
trait MetaGoal extends Goal {
	def createPrerequisiteGoals(agent: TAIAgent) = createPrerequisiteGoals()
	def createPrerequisiteGoals(): List[Goal]
}

class AIResult
object AIResult {
	case class Abort ( reason : AIReason ) extends AIResult
	case class Retry ( reason : AIReason ) extends AIResult
	case class Fail ( reason : AIReason ) extends AIResult
	case object Continue extends AIResult
	case object Success extends AIResult
}

sealed abstract class SplitResult
object SplitResult {
	case class Split ( splitOff : Goal , remainder : Goal ) extends SplitResult
	implicit class Single ( val goal : Goal ) extends SplitResult
	implicit class None ( val reason : AIReason ) extends SplitResult

	implicit def tuple2split ( tup : (Goal,Goal) ) : SplitResult = Split(tup._1,tup._2)
//	implicit def single2split ( single : Goal ) : SplitResult = Single(single)
}

sealed abstract class PrerequisiteGoalsResult {
	def isSuccess = true
}
object PrerequisiteGoalsResult {
	case class Success (goals : List[Goal] ) extends PrerequisiteGoalsResult
	case class Failure (failedGoal : Goal, reason : AIReason ) extends PrerequisiteGoalsResult { override def isSuccess = false }
	case class Possibilities (results : List[PrerequisiteGoalsResult]) extends PrerequisiteGoalsResult { override def isSuccess = results.exists(r => r.isSuccess) }
}

abstract class AIReason {
	val timestamp = GameEngine.time
	def stillApplies(world: World, agent: TAIAgent): Boolean
}

abstract class Juncture extends AIReason {
	/**
	 * A juncture can be thought of as an AI interrupt, this is the priority of that
	 * interrupt. A colony-wide order to return home for nightfall might be a mid
	 * priority interrupt. Slight hunger would be a low priority interrupt, preempting
	 * only aimless wandering. An actively attacking enemy would be a life-or-death
	 * priority interrupt, and would interrupt pretty much everything other than
	 * fleeing from another enemy, or perhaps being on fire.
 *
	 * @return
	 */
	def priority : Int
}

object CompoundAIReason {
	def apply (c : Traversable[AIReason]) = {
		c.toList match {
			case Nil => Noto.warn("Empty Compound AI Reason"); new AIReason { override def stillApplies(world: World, agent: TAIAgent): Boolean = false }
			case head :: Nil => head
			case list => new CompoundAIReason(list)
		}
	}
}

class CompoundAIReason private (reasons : List[AIReason]) extends AIReason {
	override def stillApplies(world: World, agent: TAIAgent): Boolean = reasons.forall(_.stillApplies(world, agent))
}


case object Thinking extends Goal {
	def createPrerequisiteGoals(agent:TAIAgent) = Nil
	def fitness(agent: TAIAgent): Int = 0
	def split(agent: TAIAgent): SplitResult = this
	def act(agent: TAIAgent, dt : UnitOfTime): AIResult = { AIResult.Continue }
	def progressRequired(agent: TAIAgent): Float = Float.MaxValue
}

case class WaitGoal ( duration : UnitOfTime ) extends Goal {
	def progressRequired(agent: TAIAgent): Float = duration.inSeconds
	def createPrerequisiteGoals(agent:TAIAgent) = Nil
	def split(agent: TAIAgent) = this
	var timeWaited : UnitOfTime = zeroSeconds

	def act(agent: TAIAgent, dt : UnitOfTime): AIResult = {
		progress += dt.inSeconds
		if ( progress < progressRequired(agent) ) {
			AIResult.Continue
		} else {
			AIResult.Success
		}
	}

	def fitness(agent: TAIAgent): Int = 0
}

case class ExamineGoal ( duration : UnitOfTime ) extends Goal {
	def progressRequired(agent: TAIAgent): Float = duration.inSeconds
	def createPrerequisiteGoals(agent:TAIAgent) = Nil
	def split(agent: TAIAgent) = this
	var timeWaited : UnitOfTime = zeroSeconds

	def act(agent: TAIAgent, dt : UnitOfTime): AIResult = {
		progress += dt.inSeconds
		if ( progress < progressRequired(agent) ) {
			AIResult.Continue
		} else {
			AIResult.Success
		}
	}

	def fitness(agent: TAIAgent): Int = 0
}