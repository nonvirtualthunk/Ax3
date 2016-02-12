package arx.axistential.ai

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/6/13
 * Time: 3:45 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.application.Noto
import arx.application.TLoggingLevelProvider
import arx.axistential.ai.AI.Reason.UnexpectedState
import arx.axistential.ai.AIAgentActor.RecalculateGoal
import arx.axistential.ai.AIAgentActor.RecalculatedGoalContainer
import arx.axistential.ai.AIAgentActor.Rethink
import arx.axistential.ai.AIAgentActor.RetryGoal
import arx.axistential.ai.AIResult.Continue
import arx.axistential.ai.traits.TRecalculatingGoal
import arx.axistential.game.data.entity.ClaimData
import arx.core.query.ContinuousQueryListener
import arx.core.units.UnitOfTime
import arx.tmp.game.logic.event.GameEventData
import arx.tmp.game.logic.event.TGameEvent
import arx.tmp.game.logic.world.data.TimeData

import scala.collection.mutable

class AIGameComponent extends GameEngineComponent with ContinuousQueryListener[TAIEntity] {
	canFoldUpdates = false

	val system = ActorSystem("AIActorSystem")
	val actorsByAgent = new mutable.HashMap[TAIAgent,ActorRef]()
	val actorsByGroup = new mutable.HashMap[TAIGroup,ActorRef]()

	lazy val junctureMappers = ReflectionAssistant.instancesOfSubtypesOf[TJunctureMapper]


	override def initialize(): Unit = {
		world.createEntityTypeQuery[TAIEntity].withListener(this,fireOnExistingResults = true)

		world.aux[GameEventData].postListeners ::= ((ge:TGameEvent) => {
			val res = junctureMappers.flatMap( _.toJuncture(ge) )
			for ( (agent,juncture) <- res ) {
				junctureReached(agent,juncture)
			}
			res.nonEmpty
		})
	}

	def queryResultAdded(ent: TAIEntity): Unit = {
		ent match {
			case t : TAIAgent => {
				actorsByAgent(t) = if (!gameEngine.serialMode) {
					system.actorOf(Props(classOf[AIAgentActor],t))
				} else {
					system.actorOf(Props(classOf[AIAgentActor],t).withDispatcher(CallingThreadDispatcher.Id))
				}
			}
			case g : TAIGroup => {
				actorsByGroup.getOrElseUpdate(g,if (!gameEngine.serialMode) {
					system.actorOf(Props(classOf[AIGroupActor],g))
				} else {
					system.actorOf(Props(classOf[AIGroupActor],g).withDispatcher(CallingThreadDispatcher.Id))
				})
			}
		}
	}
	def queryResultRemoved(t: TAIEntity): Unit = {
		t match {
			case a : TAIAgent => actorsByAgent.get(a) match {
				case Some(ref) => system.stop(ref)
				case None =>
			}
			case g : TAIGroup => actorsByGroup.get(g) match {
				case Some(ref) => system.stop(ref)
				case None =>
			}
		}
	}

	def junctureReached ( forAgent : TAIAgent , juncture : Juncture ) {
		junctures synchronized {
			junctures(forAgent) = junctures.getOrElse(forAgent,Set()) + juncture
		}
	}
	
	def sendMessageToActor(forAgent : TAIAgent, message : Any) {
		actorsByAgent(forAgent).tell(message,ActorRef.noSender)
	}

//	val junctures = new SynchronizedQueue[(TAIAgent,Juncture)]
	var junctures = mutable.HashMap[TAIAgent,Set[Juncture]]()

	val SomeThinking = Some(Thinking)

	def update(time: UnitOfTime): Unit = {
		val agents = world.entitiesOfType[TAIAgent]
		for ( agent <- agents if agent.actionRate > 0.0f ) {
			agent.activeGoal match {
				case Some(ag) => ag match {
					case rgc : RecalculatedGoalContainer =>
						handleRecalculatedContainer(agent,rgc)
					case _ =>
				}
				case None =>
			}

			agent.activeLeafGoal match {
				case Some(goal) =>
					if ( goal != Thinking && ! goal.isInstanceOf[RecalculatedGoalContainer] ) {
						val activeJunctures = junctures synchronized {
							val tmp = junctures.getOrElse(agent,Set())
							if ( tmp.nonEmpty ) {
								junctures(agent) = Set[Juncture]()
							}
							tmp
						}

						val goalPriority = goal.priority(agent)
						val higherPriorityJunctures = activeJunctures.filter( _.priority >= goalPriority )
						if ( higherPriorityJunctures.nonEmpty ) {
							val ag = agent.activeGoal
							agent.activeGoal = SomeThinking
							sendMessageToActor(agent,Rethink(Some(CompoundAIReason(higherPriorityJunctures)),ag))
						} else {
							val effectiveTime = if (goal.usesRawTime) { time } else { time * agent.actionRate }
							try {
								val actResult = goal.act(agent,effectiveTime)
								handleGoalResult(agent,goal,actResult)

								// We only ever want to do recalculation if we are in the middle of a goal. If the result of
								// act(...) was Success/Retry/Fail/Abort then recalculation becomes a moot point and would, in fact
								// cause undefined behavior
								if (actResult == Continue) {
									var recalcCount = 0
									// TODO: Will this ever result in multiple recalculations for the same goal hierarchy?
									goal.selfAndAllProgenitors.foreach {
										case recalc : TRecalculatingGoal if ! recalc.recalculationRequested && recalc.needsRecalculation(agent) =>
											recalcCount += 1
											recalc.recalculationRequested = true
											sendMessageToActor(agent,RecalculateGoal(recalc,agent.activeGoal.get))
										case _ =>
									}
									if (recalcCount > 1) {
										Noto.warn("Multiple recalculations in a single goal hierarchy, are we ok with this?")
									}
								}
							}
							catch {
								case e : Exception => {
									Noto.warn("Exception encountered while calling Goal.act(...)." +
										s"\n\tproblematic goal was $goal" +
										s"\n\texception was $e")
									e.printStackTrace()
									goal.aborted(agent,UnexpectedState("Exception"))
									val ag = agent.activeGoal
									agent.activeGoal = SomeThinking
									sendMessageToActor(agent,Rethink(Some(UnexpectedState("Exception")),ag))
								}
							}
						}
					}
				case None =>
					agent.activeGoal = SomeThinking
					sendMessageToActor(agent,Rethink(None,None))
			}
		}

		val currentTime = world.aux[TimeData].time
		val groups = world.entitiesOfType[TAIGroup]
		for (group <- groups) {
			for (groupGoal <- group.groupGoals if (currentTime - groupGoal.lastChecked) >= groupGoal.checkInterval) {
				groupGoal.activeSubGoal match {
					case Some(sg) =>
						if (sg.finished) {
							Noto.finest(AILogging,f"GroupGoal $groupGoal, subgoal finished, clearing")
							groupGoal.activeSubGoal = None
						}
					// do nothing, it's already got something going on
					case None => {
						groupGoal.lastChecked = currentTime
						groupGoal.createSubGoal(group) match {
							case Some(sg) => {
								Noto.finest(AILogging,f"GroupGoal $groupGoal generated new subgoal: $sg")
								groupGoal.activeSubGoal = Some(sg)
								group.addGoal(sg)
							}
							case None => // do nothing
						}
					}
				}
			}
		}
	}

	def handleGoalResult (agent : TAIAgent,goal : Goal, result : AIResult) = result match {
		case AIResult.Success => {
			clearClaims(agent,goal)

			goal.markFinished()
			if ( agent.activeGoal == Some(goal) ) { //if this is top level
				agent.activeGoal = SomeThinking
				sendMessageToActor(agent,Rethink(None,None))
			}
		}
		//carry on, nothing to see here
		case AIResult.Continue =>
		//Something has gone pear shaped on us, but we don't have any reason to believe the goal
		//is fundamentally wrong, maybe something moved out from under us, or a precondition changed.
		//Retry should trigger a reset() on the goal, as well as the recondition of prerequisite/sub goals.
		//At some point we may include a "retried" counter, and escalate to an abort if retry is
		//hit too many times in a row, help us break out of tricksy behavioral loops there
		case AIResult.Retry(reason) => {
			val ag = agent.activeGoal.get
			if (ag == goal) {
				Noto.fine(AILogging,s"Retrying goal: $ag\n\treason : $reason")
			} else {
				AILogging.logFullHierarchyFailure("Retrying",goal,reason)
			}
			agent.activeGoal = SomeThinking
			sendMessageToActor(agent,RetryGoal(goal,ag))
		}
		//something about this goal makes it impossible to do correctly at this time; a goal to move to an
		//inaccessible location, or craft an item for which the materials are not available
		case AIResult.Abort(reason) => {
			//In this case, we want to record what it was that caused the issue so that we can later
			//determine whether or not it still applies. That should help us avoid head-wall-repeat.
			agent.activeGoal match {
				case Some(ag) => {
					goal.aborted(agent,reason)
					AILogging.logFullHierarchyFailure("Aborted",goal,reason)
					// This was not present originally, but I don't know why we wouldn't want it
					clearClaims(agent,goal)
					agent.activeGoal = SomeThinking
					sendMessageToActor(agent,Rethink(Some(reason),Some(ag)))
				}
				case _ => Noto.warn("Abort result when agent had no active goal")
			}
		}
		// This goal has been determined to be permanently impossible, or invalid; a goal to heal
		// 	a dead person, or research a spell that is already known, etc.
		// It is important to note that a failure at one level should be interpreted as a temporary abort
		// 	at the level above. The fact that a ChaseGoal failed because the target disappeared does
		// 	not mean that hunting as a whole is impossible.
		case AIResult.Fail(reason) => {
			goal.failed(agent,reason)
			AILogging.logFullHierarchyFailure("Failed",goal,reason)
			clearClaims(agent,goal)
			val ag = agent.activeGoal
			agent.activeGoal = SomeThinking
			// If there is no parent goal then this should legitimately be discarded and not replaced, the whole
			// 	concept is invalid and should not be revisited
			if (goal.parentGoal.isEmpty) {
				sendMessageToActor(agent,Rethink(None,None))
			// Otherwise we should treat this as an abort of the goal overall
			} else {
				sendMessageToActor(agent,Rethink(Some(reason),ag))
			}
		}
	}

	/** Collapses a recalculated goal container back into a normal goal hierarchy */
	def handleRecalculatedContainer(agent : TAIAgent, rgc: RecalculatedGoalContainer) : Goal = {
		rgc.recalculationResult match {
			case AIResult.Success =>
				val newMain = graft(rgc.newGoal,rgc.oldGoal,rgc.oldMain)
				agent.activeGoal = Some(newMain)
				clearClaims(agent,rgc.oldGoal)
				rgc.newGoal
			case ab : AIResult.Abort =>
				val goal = rgc.oldGoal
				goal.aborted(agent,ab.reason)
				AILogging.logFullHierarchyFailure("(recalc) Aborted",goal,ab.reason)
				clearClaims(agent,goal)
				agent.activeGoal = SomeThinking
				sendMessageToActor(agent,Rethink(Some(ab.reason),Some(goal.topLevelParent)))

				Thinking
			case af : AIResult.Fail =>
				handleGoalResult(agent,rgc.oldGoal,af)
				Thinking
			case re : AIResult.Retry =>
				handleGoalResult(agent,rgc.oldGoal,re)
				Thinking
		}
	}

	def clearClaims ( agent : TAIAgent, goal : Goal ) {
		goal.claimedEntities.foreach( ent => ent.aux[ClaimData].unclaim(agent,goal) )
		goal.claimedEntities = Nil
		goal.subGoals.foreach( g => clearClaims(agent,g) )
	}


	/** Grafts the given goal into the goal tree represented by oldGoal/mainGoal. Returns the new top level
	  * mainGoal, which will either be the same as it was, or replaced with the newGoal  */
	def graft (newGoal : TRecalculatingGoal, oldGoal : TRecalculatingGoal, mainGoal : Goal) = {
		oldGoal.parentGoal match {
			case Some(parent) =>
				val oldIndex = parent.subGoals.indexOf(oldGoal)
				parent.subGoals = parent.subGoals.updated(oldIndex,newGoal)
				newGoal.parentGoal = Some(parent)
				mainGoal
			case None =>
				newGoal
		}
	}

	override def exit(): Unit = {
		system.shutdown()
		system.awaitTermination()
	}
}

object AILogging extends TLoggingLevelProvider {
	override def loggingLevel: Int = overrideLoggingLevel.getOrElse(System.getProperty("ailogging","0").toInt)
	private var overrideLoggingLevel : Option[Int] = None
	def setLoggingLevel ( level : Int ): Unit = {
		overrideLoggingLevel = Some(level)
	}

	def logFullHierarchyFailure (messageStart: String,goal : Goal, reason : AIReason): Unit = {
		var goalHierarchy = List[Goal]()
		var tmp = goal
		while (tmp != null) {
			goalHierarchy ::= tmp
			tmp = tmp.parentGoal.orNull
		}

		if (goalHierarchy.size > 1) {
			var t = ""
			for (g <- goalHierarchy) {
				if (t == "") { Noto.fine(AILogging,s"$messageStart goal: $g") }
				else { Noto.fine(AILogging,s"${t}for subgoal: $g") }
				t = t + "\t"
			}
			Noto.fine(AILogging,s"${t}reason: $reason")
		} else {
			Noto.fine(AILogging,s"$messageStart goal: $goal, reason: $reason")
		}
	}
}

class AIGroupActor ( agent : TAIGroup) extends Actor {
	override def receive: Actor.Receive = {
		case "" =>
	}
}

class AIAgentActor ( agent : TAIAgent ) extends Actor {
	def goalSort (agent : TAIAgent)( g1 : Goal , g2 : Goal ) = {
		val p1 = g1.priority(agent)
		val p2 = g2.priority(agent)
		if ( p1 > p2 ) {
			true
		} else if ( p1 < p2 ) {
			false
		} else {
			val f1 = g1.fitness(agent)
			val f2 = g2.fitness(agent)
			f1 > f2
		}
	}

	def returnGoalToGroup ( goal : Goal ) {
//		if ( goal._removed ) { //If this has not been returned to the group in the meantime (but what if it has been returned, then taken again?)
		goal.reset()
		clearClaims(goal)
		goal.subGoals = Nil
		if ( goal._fromGroup ) { agent.aiGroup.addGoal(goal) }
//		}
	}
	def clearClaims ( goal : Goal ) {
		goal.claimedEntities.foreach( ent => ent.aux[ClaimData].unclaim(agent,goal) )
		goal.claimedEntities = Nil
		goal.subGoals.foreach( clearClaims )
	}

	var waitCount = 0
	def createWaitGoal () = {
		val w = 0.05f + waitCount.toFloat * 0.25f + rand(0.01f,0.1f)
		waitCount += 1
		WaitGoal(w.seconds)
	}
	def resetWaitCount () { waitCount = 0 }

	def receive: Actor.Receive = {
		case Rethink ( reasonOpt , previousGoal ) => {
			val world = agent.world

			if (reasonOpt.isEmpty && previousGoal.nonEmpty && ! previousGoal.get.finished) {
				Noto.warn("This was previously unhandled, a goal that needed a rethink but was never returned to group")
				returnGoalToGroup(previousGoal.get)
			}

			//If there was a reason for this rethinking, and there is an active, unfinished goal
			for ( reason <- reasonOpt ; goal <- previousGoal if ! goal.finished ) {
				goal.interruptedBy ::= reason
				returnGoalToGroup(goal)
				for ( splitFrom <- goal._splitFrom ) {
					splitFrom synchronized {
						//Basically, if this isn't a goal that another agent is currently pursuing, in which case there's no real point
						//in interrupting it, they'll see what's going on soon enough. Now, this won't work quite right if the goal
						//has been split more recently, we'll be marking a no longer relevant goal, but that should be ok
						if ( ! splitFrom._removed ) {
							splitFrom.interruptedBy ::= reason
						}
					}
				}
			}

			var assignedGoal : Goal = null

			do {
				val baseGoals = (agent.passiveGoals ::: agent.aiGroup.goals).sortWith( goalSort(agent) )
				val possibleGoals = baseGoals.dropWhile( g =>
					! g.isReady(agent) ||
					! g.isActive(world) ||
					g.isInterrupted(world, agent) ||
					g.subGoals.exists( ! _.finished ) ||
					g.waitingOn.exists( ! _.finished) ||
					! g.isValidAgent(agent)
				)

				assignedGoal = possibleGoals match {
					case Nil =>
						Noto.finest(AILogging,f"No active goals found for agent $agent")
						createWaitGoal()
					case bestGoal :: _ => {
						if ( bestGoal.priority(agent) == AI.Priority.Minimum ) {
							Noto.fine(AILogging,s"Highest priority was a minimum, just wait instead")
							createWaitGoal()
						} else {
							resetWaitCount()
							bestGoal
						}
					}
				}

				assignedGoal.synchronized {
					if ( assignedGoal._removed ) { //someone split it or took it while we were busy
						assignedGoal = null
					} else {
						if ( assignedGoal._fromGroup ) {
							assignedGoal._removed = true
							agent.aiGroup.removeGoal( assignedGoal )
							agent.aiGroup.setGoalToStaging( assignedGoal )
						}
					}
				}
			} while ( assignedGoal == null )


			var continue = true
			var nextGoal = assignedGoal
			do {
				val split = splitGoal(nextGoal)
				split match {
					case single : SplitResult.Single => {
//						Noto.fine(AILogging,"Splitting single : " + single.goal)
						if (! (single.goal eq nextGoal)) {
							nextGoal._splitInto = Some(single.goal)
						}
						setUpAssignedGoal(single.goal,single.goal)
						continue = false
					}
					case SplitResult.Split(splitOff,remainder) => {
						splitOff._splitFrom = Some(remainder)
						nextGoal._splitInto = Some(List(splitOff,remainder))

						nextGoal = splitOff
						returnGoalToGroup(remainder)
					}
					case n : SplitResult.None => {
						assignedGoal.interruptedBy ::= n.reason
						returnGoalToGroup(assignedGoal)
						agent.activeGoal = None
						continue = false
					}
				}
			} while ( continue )

			agent.aiGroup.removeGoalFromStaging( assignedGoal )
		}
		case RetryGoal ( goal , mainGoal ) => {
			Noto.fine(s"Retrying goal : $goal")
			setUpAssignedGoal(goal,mainGoal)
		}
		case RecalculateGoal (goal, mainGoal) => {
			Noto.fine(s"Recalculating goal : $goal")
			val newInst = goal.createTopLevelCopy
			val rgc = recalculateGoal(newInst,goal,mainGoal)

			// TODO: Work out the exact details of the concurrency here. When recalculating the active goal will be
			// happily ticking along, we only want to replace it with our recalced one if it hasn't changed significantly
			// since. The exact timing gives concern.

			// Only swap in our recalculated goal if the original one hasn't been supplanted
			if (agent.activeGoal == Some(mainGoal)) {
				agent.activeGoal = Some(rgc)
			}
		}
	}



	def splitGoal ( goal : Goal ) = {
		val ret = goal.split(agent)
		ret match {
			case s : SplitResult.Split =>
				s.splitOff._fromGroup = goal._fromGroup
				s.remainder._fromGroup = goal._fromGroup
				s.splitOff._priority = goal._priority
				s.remainder._priority = goal._priority
			case _ =>
		}

		ret
	}

	protected def createPrerequisiteGoalsRecursive ( goal : Goal ) : PrerequisiteGoalsResult = {
		goal.createPrerequisiteGoals(agent) match {
			// collapse the multiple possibilities case
			case PrerequisiteGoalsResult.Possibilities(results) => {
				results.find(_.isSuccess) match {
					case None => results.head
					case Some(success) => interpretPrerequisiteGoalsRecursive(goal,success)
				}
			}
			case other => interpretPrerequisiteGoalsRecursive(goal,other)
		}
	}
	protected def interpretPrerequisiteGoalsRecursive (goal : Goal, result : PrerequisiteGoalsResult) :PrerequisiteGoalsResult = {
		result match {
			case success : PrerequisiteGoalsResult.Success =>
				// Explode out all the splits, prerequisites will not be distributed among other agents, all will be done by this agent
				val explodedGoals = success.goals.flatMap(g => g.allSplits(agent))
				explodedGoals.foreach( _.parentGoal = Some(goal) )
				goal.subGoals = explodedGoals
				for ( subGoal <- goal.subGoals ) {
					createPrerequisiteGoalsRecursive(subGoal) match {
						case subSuccess : PrerequisiteGoalsResult.Success => //carry on
						case subFailure : PrerequisiteGoalsResult.Failure => return subFailure
					}
				}
				success
			case failure : PrerequisiteGoalsResult.Failure =>
				failure
		}
	}

	def setUpAssignedGoal ( goal : Goal , mainGoal : Goal ) = {
		goal.reset()
		goal.claimedEntities.foreach( ent => ent.aux[ClaimData].unclaim(agent,goal) )

		val prereqResult = createPrerequisiteGoalsRecursive(goal)
		prereqResult match {
			case success : PrerequisiteGoalsResult.Success => {
				val planningResult = goal.planRecursive(agent)
				planningResult.result match {
					case AIResult.Success =>
						agent.activeGoal = Some(mainGoal)
					case AIResult.Fail(planningFailureReason) =>
						// TODO : should we have a "goal.failed(agent)" here as well?
						goal.interruptedBy ::= planningFailureReason
						returnGoalToGroup(mainGoal)
						AILogging.logFullHierarchyFailure("Failed to plan goal",planningResult.fromGoal,planningFailureReason)
						planningResult.fromGoal.planningFailed(agent,planningFailureReason)
						agent.activeGoal = None
					case AIResult.Abort(planningFailureReason) =>
						goal.interruptedBy ::= planningFailureReason
						returnGoalToGroup(mainGoal)
						AILogging.logFullHierarchyFailure("Abort on plan goal",planningResult.fromGoal,planningFailureReason)
						planningResult.fromGoal.planningAborted(agent,planningFailureReason)
						agent.activeGoal = None
					case AIResult.Retry(retryReason) =>
						goal.interruptedBy ::= retryReason
						returnGoalToGroup(mainGoal)
						AILogging.logFullHierarchyFailure("Retry on plan goal, treated as an abort",planningResult.fromGoal,retryReason)
						planningResult.fromGoal.planningAborted(agent,retryReason)
						agent.activeGoal = None
				}
			}
			case failure : PrerequisiteGoalsResult.Failure => {
				goal.interruptedBy ::= failure.reason
				returnGoalToGroup(mainGoal)
				AILogging.logFullHierarchyFailure("Failed to create prerequisite goals on",failure.failedGoal,failure.reason)
				agent.activeGoal = None
			}
		}
	}

	def recalculateGoal ( goal : TRecalculatingGoal , oldGoal : TRecalculatingGoal, mainGoal : Goal ) = {
		val prereqResult = createPrerequisiteGoalsRecursive(goal)
		prereqResult match {
			case success : PrerequisiteGoalsResult.Success => {
				val planningResult = goal.planRecursive(agent)

				new RecalculatedGoalContainer(goal,oldGoal,mainGoal, planningResult.result)
			}
			case failure : PrerequisiteGoalsResult.Failure => {
				new RecalculatedGoalContainer(goal,oldGoal,mainGoal, AIResult.Abort(failure.reason))
			}
		}
	}
}

object AIAgentActor {
	case class Rethink ( reason : Option[AIReason] , previouslyActiveGoal : Option[Goal] )
	case class RetryGoal ( goal : Goal, mainGoal : Goal )
	case class RecalculateGoal ( goal : TRecalculatingGoal, mainGoal : Goal )

	class RecalculatedGoalContainer (val newGoal : TRecalculatingGoal, val oldGoal : TRecalculatingGoal, val oldMain : Goal, val recalculationResult : AIResult) extends Goal {
		override def createPrerequisiteGoals(agent: TAIAgent): PrerequisiteGoalsResult = Nil
		override def progressRequired(agent: TAIAgent): Float = 0
		override def split(agent: TAIAgent): SplitResult = this
		override def fitness(agent: TAIAgent): Int = 0
		override def act(agent: TAIAgent, dt: UnitOfTime): AIResult =
			Fail(AI.Reason.UnexpectedState("recalculated goal container should never actually be called"))
	}
}



