package arx.eldr.game.logic.ai

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.units.UnitOfTime
import arx.eldr.game.entity.data.AIAgentData
import arx.engine.entity.GameEntity
import arx.engine.entity.TGameEntity
import arx.engine.game.GameEngine
import arx.engine.game.components.GameComponent

class AIGameComponent(eng: GameEngine) extends GameComponent(eng) {
	val groupQuery = world.auxDataQuery[AIGroupData]
	val agentQuery = world.auxDataQuery[AIAgentData]

	override protected def update(dt: UnitOfTime): Unit = {
		for (group <- groupQuery) {
			val GD = group[AIGroupData]

			var continue = true
			while (continue) {
				val idleEntities = GD.entities.filter(e => e[AIAgentData].goals.isEmpty)
				val openGoals = GD.goals.filter(isValidAndRead)

				if (idleEntities.isEmpty || openGoals.isEmpty) {
					continue = false
				} else {
					val chosenGoal = openGoals.maxBy(g => effectivePriority(g).value)
					val bestEntity = idleEntities.maxBy(e => fitnessForGoal(e, chosenGoal))

					assignGoalToEntity(group, bestEntity, chosenGoal)
				}
			}
		}

		for (agent <- agentQuery) {
			val AD = agent.aux[AIAgentData]
			if (AD.goals.nonEmpty) {
				val activeTopLevelGoal = AD.goals.maxBy(g => goalSort(agent, g))

				if (!activeTopLevelGoal.prerequisitesCreated) {
					createAllPrerequisiteGoals(agent, activeTopLevelGoal) match {
						case AIResult.Success =>
							activeTopLevelGoal.prerequisitesCreated = true
						case AIResult.Abort(reason) =>
							Noto.warn(s"Aborting goal $activeTopLevelGoal during prerequisite creation: $reason")
							resetAndRemoveGoal(agent, activeTopLevelGoal, reason, returnToGroup = true)
						case AIResult.Fail(reason) =>
							Noto.warn(s"Failing goal $activeTopLevelGoal during prerequisite creation: $reason")
							resetAndRemoveGoal(agent, activeTopLevelGoal, reason, returnToGroup = false)
						case AIResult.Retry(reason) =>
							activeTopLevelGoal.interruptedBy ::= reason
							reset(agent,activeTopLevelGoal)
					}
				}

				if (activeTopLevelGoal.prerequisitesCreated) {
					val activeLeafGoal = leafGoalFor(activeTopLevelGoal)
					posit(!activeLeafGoal.complete, "Leaf goal already complete, but still set")

				// if we got through our prerequisite creation we can actually try planning

					// TODO: Split onto separate threads
					if (!activeLeafGoal.planned) {

						planAll(agent, activeLeafGoal) match {
							case AIResult.Success =>
								activeLeafGoal.planned = true
							case AIResult.Abort(reason) =>
								Noto.warn(s"Aborting goal $activeLeafGoal for reason $reason")
								resetAndRemoveGoal(agent, activeTopLevelGoal, reason, returnToGroup = true)
							case AIResult.Fail(reason) =>
								Noto.warn(s"Failing goal $activeLeafGoal for reason $reason")
								resetAndRemoveGoal(agent, activeTopLevelGoal, reason, returnToGroup = false)
							case AIResult.Retry(reason) =>
								activeLeafGoal.interruptedBy ::= reason
								reset(agent,activeLeafGoal)
						}
					}

					//TODO: Prereq and postreq

					if (activeLeafGoal.planned) {
						activeLeafGoal.act(agent, dt) match {
							case AIResult.Success =>
								activeLeafGoal.complete = true
								activeLeafGoal.onGoalEnded(agent)
							case AIResult.Continue =>
							// do nothing
							case AIResult.Abort(reason) =>
								resetAndRemoveGoal(agent, activeTopLevelGoal, reason, returnToGroup = true)
							case AIResult.Fail(reason) =>
								resetAndRemoveGoal(agent, activeTopLevelGoal, reason, returnToGroup = false)
							case AIResult.Retry(reason) =>
								reset(agent,activeLeafGoal)
						}
					}
				}

				if (activeTopLevelGoal.complete) {
					AD.goals -= activeTopLevelGoal
					//TODO: Record completion somewhere
				}
			}
		}
	}

	def reset (agent : TGameEntity, goal : Goal): Unit = {
		goal.prerequisiteGoals = Nil
		goal.planned = false
		goal.prerequisitesCreated = false
		goal.onGoalEnded(agent)
	}

	def createAllPrerequisiteGoals(agent : TGameEntity, goal : Goal) : AIResult = {
		val preRes = goal.createPrerequisiteGoals(agent)
		preRes.result match {
			case AIResult.Success =>
				val subSubRes = preRes.goals.map(g => createAllPrerequisiteGoals(agent,g))
				val subSubFailures = subSubRes.filter(_ != AIResult.Success)
				subSubFailures match {
					case Nil =>
						goal.prerequisiteGoals = preRes.goals
					case firstFailure :: _ =>
						return firstFailure
				}
			case AIResult.Abort(reason) =>
				Noto.warn(s"Aborting goal $goal for reason $reason on creation of subgoal")
				resetAndRemoveGoal(agent,goal,reason,returnToGroup = true)
			case AIResult.Fail(reason) =>
				Noto.warn(s"Failing goal $goal for reason $reason on creation of subgoal")
				resetAndRemoveGoal(agent,goal,reason,returnToGroup = false)
			case AIResult.Retry(reason) =>
				Noto.warn(s"Retrying goal $goal for reason $reason on creation of subgoal")
				goal.interruptedBy ::= reason
		}

		preRes.result
	}

	def isValidAndRead(goal: Goal): Boolean = true

	def effectivePriority(goal: Goal): GoalPriority = goal.priority

	def fitnessForGoal(agent: TGameEntity, goal: Goal): Float = rand(0.0f, 1.0f)

	def goalSort(agent: TGameEntity, g: Goal): Float = g.priority.value - g.interruptedBy.size * 50

	def resetAndRemoveGoal(agent: TGameEntity, goal: Goal, reason: AIReason, returnToGroup: Boolean) = {
		reset(agent,goal)
		goal.interruptedBy ::= reason
		agent[AIAgentData].goals -= goal

		if (returnToGroup) {
			// if it's from a group move it back to that group
			for (group <- goal.fromGroup) {
				agent[AIAgentData].goals -= goal
				group[AIGroupData].goals += goal
			}
		}
	}

	def leafGoalFor(goal: Goal): Goal = {
		goal.leafGoal
	}

	def planAll(agent: TGameEntity, goal: Goal) = {
		goal.plan(agent)
	}

	def assignGoalToEntity(group: TGameEntity, agent: TGameEntity, goal: Goal) = {
		val GD = group[AIGroupData]
		val AD = agent[AIAgentData]

		val effectiveGoal = goal.split(agent) match {
			case n: SplitResult.None => Noto.error("Failed to split, should actually implement handling of that"); goal
			case s: SplitResult.Single => s.goal
			case m: SplitResult.Split =>
				GD.goals += m.remainder
				GD.goals += m.splitOff
				m.splitOff
		}

		effectiveGoal.fromGroup = Some(group)
		GD.goals -= effectiveGoal
		AD.goals += effectiveGoal
	}
}
