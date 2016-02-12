package arx.axistential.game.logic.ai.goals.animals

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 3/28/15
 * Time: 12:08 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.ai._
import arx.axistential.game.data.entity.AnimalAIData
import arx.axistential.game.data.entity.AnimalAIData.Patch
import arx.axistential.game.logic.ai.AnimalAIUtil
import arx.core.units.UnitOfTime

abstract class PatchBasedGoal extends PhysicalGoal {
	def patchCooldownTime(agent:TAIAgent) : UnitOfTime
	def goalForCurrentPatch (agent : TAIAgent, currentPatch : Patch) : Option[Goal]
	def expectedPatchYield (agent : TAIAgent, AD : AnimalAIData, currentPatch : Patch, targetPatch : Patch) : Float
	def finishWithPatch (agent : TAIAgent, patch : Patch)

	override def createPrerequisiteGoals(agent: TAIAgent): PrerequisiteGoalsResult = {
		val AD = agent.aux[AnimalAIData]
		val examinationRadius = AD.patchSize

		val currentPatch = AD.patchFor(agent.adjustedFootVoxelPos,examinationRadius)

		goalForCurrentPatch(agent,currentPatch) match {
			case Some(g) =>
				g :: Nil
			case _ => {
				finishWithPatch(agent,currentPatch)

				val (bestPatch,bestYield) = AD.patches.maxByWithValue(patch => expectedPatchYield(agent,AD,currentPatch,patch))
				if (bestYield > 0.0f) {
					Noto.finest(AILogging,s"Returning to patch with expected forage: $bestPatch")
					// Move to a new, known good region to look for some food
					ExamineGoal(rand(1,3).seconds) :: new MoveToPatchGoal(Some(currentPatch),bestPatch) :: Nil
				} else {
					//						// Try to find a new patch
					AnimalAIUtil.pickNewPatch(agent) match {
						case Some(newPatch) => {
							Noto.finest(AILogging,s"Exploring new patch : $newPatch")
							ExamineGoal(rand(2,5).seconds) :: new MoveToPatchGoal(Some(currentPatch),newPatch) :: Nil
						}
						case None => {
							val goalCooldown = patchCooldownTime(agent) / 4.0f
							Noto.finest(AILogging,s"Best available patch still has a negative expected yield, no useful forage, will retry in $goalCooldown")
							AI.Reason.NoValidTarget(goalCooldown)
						}
					}
				}
			}
		}
	}

}
