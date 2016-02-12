package arx.axistential.ai

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/6/13
 * Time: 4:32 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.core.representation.InformationLevel.InformationLevel
import arx.core.traits.TSentinel
import arx.core.traits.TSentinelable
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.data.TGameEntityAuxData
import arx.tmp.game.networking.TNetworkedGameEntityAuxData
import arx.macros.NetworkedAuxData

trait TAIGroup extends GameEntity with TAIEntity with TSentinelable {
	protected[ai] var internGroupData = createInternGroupData()

	def createInternGroupData () = {
		val ad = new AIGroupData()
		addAuxData(ad)
		ad
	}

	override protected def onNewAuxDataCreated(gead: TGameEntityAuxData): Unit = {
		super.onNewAuxDataCreated (gead)

		gead match {
			case aid : AIGroupData => internGroupData = aid
			case _ =>
		}
	}

	def addEntity ( entity : GameEntity): Unit = {
		internGroupData.addEntity(entity,this)
	}

	def addGoal ( goal : Goal ): Unit = {
		internGroupData.addGoal(goal,this)
	}
}

object TAIGroup {
	val Sentinel : TAIGroup = new TAIGroup with TSentinel {
		protected def readResolve : Object = TAIGroup.Sentinel
	}

	implicit def toGroupData (group : TAIGroup) : AIGroupData = group.internGroupData
}

trait PassiveGoalCreator extends ((TAIAgent) => List[PassiveGoal]) {
	def createPassiveGoalsFor (agent : TAIAgent) : List[PassiveGoal]

	override def apply(v1: TAIAgent): List[PassiveGoal] = createPassiveGoalsFor(v1)
}

@NetworkedAuxData
class AIGroupData extends TGameEntityAuxData with TNetworkedGameEntityAuxData {
	protected var _agents = List[TAIAgent]()
	protected var _entities = Set[GameEntity]()

	protected var _goals = List[Goal]()
	protected var _stagingGoals = Set[Goal]()
	protected var _groupGoals = List[AIGroupGoal]()
	/** Allows passive goals to be automatically assigned to members of this group */
	protected var passiveGoalCreators = List[(TAIAgent) => List[PassiveGoal]]()



	def goals = _goals
	def stagingGoals = _stagingGoals
	def groupGoals = _groupGoals


	def addPassiveGoalCreator (pgc : (TAIAgent) => List[PassiveGoal]) {
		for (agent <- _agents ; pgoal <- pgc(agent)) { agent.passiveGoals ::= pgoal }
		passiveGoalCreators ::= pgc
	}


	def addEntity ( entity : GameEntity, thisGroup : TAIGroup ) {
		for (agent <- entity.ifIs[TAIAgent]) {
			addAgent(agent,thisGroup)
		}
		_entities += entity
		fieldModified()
	}
	def entities = _entities

	def addGroupGoal (groupGoal : AIGroupGoal): Unit = {
		_groupGoals ::= groupGoal
		fieldModified()

	}

	def addGoal ( goal : Goal, thisGroup : TAIGroup ) {
		if (goal.checkSanity()) {
			goal.synchronized {
				if ( ! goal._fromGroup ) {
					goal.onAdded(thisGroup)
				}
				goal._removed = false
				goal._fromGroup = true
				_goals.synchronized {
					_goals = (goal :: _goals).sortBy( _._priority.resolve() * -1 )
				}

				goal match {
					case mg : MetaGoal => {
						val pregoals = mg.createPrerequisiteGoals()
						mg.subGoals = pregoals
						pregoals.foreach(pg => addGoal(pg,thisGroup))
					}
					case _ =>
				}
			}
			fieldModified()
		}
	}

	def setGoalToStaging ( goal : Goal ) {
		_stagingGoals += goal
	}
	def removeGoalFromStaging (goal : Goal) {
		_stagingGoals -= goal
	}
	def removeGoal ( goal : Goal ) {
		goal._removed = true
		_goals.synchronized {
			_goals = _goals without goal
		}
		fieldModified()
	}

	protected def addAgent ( agent : TAIAgent, thisGroup : TAIGroup ) {
		_agents ::= agent
		for (pgc <- passiveGoalCreators ; pgoal <- pgc(agent)) { agent.passiveGoals ::= pgoal }
		agent.aiGroup = thisGroup
		fieldModified()
	}

	override def informationPairs(level: InformationLevel): Map[String, Any] = Map()
}