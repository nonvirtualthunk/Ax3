package arx.axistential.game.logic.ai.goals.animals

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/10/15
 * Time: 2:50 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.ai._
import arx.axistential.game.archetypes.Covering
import arx.axistential.game.archetypes.species.AnimalSpecies
import arx.axistential.game.archetypes.species.PlantSpecies
import arx.axistential.game.data.entity.AnimalAIData.Patch
import arx.axistential.game.data.entity.animal.AnimalData
import arx.axistential.game.data.entity.animal.DietType
import arx.axistential.game.data.entity.animal.EatingData
import arx.axistential.game.data.entity.plants.PlantData
import arx.axistential.game.data.entity.AnimalAIData
import arx.axistential.game.data.entity.FoodData
import arx.axistential.game.data.world.PhysicalEntityLocationData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.entities.CreatureEntity
import arx.axistential.game.logic.ai.AnimalAIUtil
import arx.axistential.game.logic.ai.AxisSearcher
import arx.axistential.game.logic.general.CorpseEntity
import arx.axistential.game.logic.general.MovementLogic
import arx.axistential.game.world.AxistentialWorld.AxistentialWorld
import arx.axistential.modules.hunger.ActivityLevel
import arx.axistential.modules.hunger.MetabolicData
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.coordinates.VoxelSideCoord
import arx.tmp.game.logic.descriptors.TDescriptor

import scala.language.postfixOps
import scalaxy.loops._

class ForageGoal extends PatchBasedGoal with PassiveGoal with PhysicalGoal {
	override def activityLevel: ActivityLevel = ActivityLevel.Light
	var totalCals = 0.0f

	val defaultPatchExpectation = 150.0f

	def patchCooldownTime(agent : TAIAgent) = agent.aux[EatingData].dietType match {
		case DietType.Herbivore => 1.cycle
		case DietType.Carnivore => 0.35.cycle
		case _ => 0.5.cycle
	}


	override def priority(agent: TAIAgent): Int = super.priority(agent)


	override def goalForCurrentPatch(agent : TAIAgent, currentPatch: Patch): Option[Goal] = {
		val PELD = agent.world.aux[PhysicalEntityLocationData]
		val eatingData = agent.aux[EatingData]
		val nearbyEntities = PELD.entitiesInRegion(currentPatch.region,exact = false)

		if (eatingData.dietType == DietType.Herbivore) {
			val nearbyPlants = nearbyEntities.filter(p => p.archetype.isInstanceOf[PlantSpecies])

			// Filter out plants that are in threatened areas or that we expect to be close to a predator
			// Prevents ping-ponging
			val nonThreatenedPlants = nearbyPlants.filterNot(p => AnimalAIUtil.isLocationThreatening(agent,p.position))

			val plantsToEdibleParts = nonThreatenedPlants
				.map(pl => pl -> pl.aux[PlantData].parts.filter(part => eatingData.canEatEntity(part.entity)))
			val (plantsWithEdibleParts,_) = plantsToEdibleParts.filter(_._2.nonEmpty).unzip
			val allEdibleParts = plantsToEdibleParts.flatMap(_._2)

			if (plantsWithEdibleParts.nonEmpty) {
				val expectedCalories = allEdibleParts.fsum(p => p.entity.aux[FoodData].calories)

				currentPatch.forage.currentForageCalories += expectedCalories
				totalCals += expectedCalories
				Noto.finest(AILogging,s"Attempting grazing on some plants, expected calorie gain : $expectedCalories, theoretical toal : $totalCals")

				Some(new GrazePlantsGoal(plantsWithEdibleParts,eatingData.canEat))
			} else {
				// If we didn't find any handy big plants that we want to eat, check out any nearby groundcover

				val TD = agent.world.aux[TerrainData]
				val ED = agent.aux[EatingData]

				def desirabilityFunc (v : VoxelCoord) = {
					var found = false
					for (q <- 0 until 6 optimized) {
						val covering = TD.coveringAt(v,q)
						// If we have an actual covering and it hasn't been eated already
						found ||= ForageGoal.canAgentEatCovering(ED.canEat,covering)
					}
					if (found) { 1.0f } else { 0.0f }
				}

				AxisSearcher.pathTo(AxistentialFloodSearchQuery(
					agent,
					1.0f,
					currentPatch.region.boundingDimensions.max.inVoxels.toInt, // if it costs more than the patch's dimensions, maybe don't do it
					MovementLogic.moveCostFunction(agent),
					desirabilityFunc,
					MovementLogic.obstructionFunction(agent.world),
					MovementLogic.isSupportedFunction(agent.world),
					(v) => currentPatch.region.contains(v)
				)) match {
					case Some(vPath) if vPath.size > 0 =>
						val v = vPath.last
						val allCoords = (0 until 6).map(q => VoxelSideCoord(v,q))
						val edibleCoords = allCoords.filter(vs => ForageGoal.canAgentEatCovering(ED.canEat,TD.coveringAt(vs)))
						if (edibleCoords.nonEmpty) {
							Some(new GrazeGroundGoal(edibleCoords))
						} else {
							None
						}
					case _ => None
				}
			}
		} else if (eatingData.dietType == DietType.Carnivore) {
			// Eat nearby dead animal if there is one available
			val nearbyCorpses = nearbyEntities
				.ofType[CorpseEntity]

			val ediblePartsByCorpse = nearbyCorpses
				.map(c => c -> c.aux[AnimalData].parts.filter(_.entity.hasAuxData[FoodData]))
				.filter { case (critter,critterParts) => critterParts.nonEmpty }
				.toMap

			if (ediblePartsByCorpse.nonEmpty) {
				val (target,parts) = ediblePartsByCorpse.maxBy { case (ent,entParts) => entParts.map(p => p.entity.aux[FoodData].calories).sum }
				Some(new EatAnimalGoal(target))
			} else {
				// Otherwise try and find a nearby animal to make dead
				val nearbyAnimals = nearbyEntities
					.ofType[CreatureEntity]
					.filter(p => p.archetype.isInstanceOf[AnimalSpecies])
					.filter(a => a.aiGroup != agent.aiGroup || agent.aiGroup.isSentinel) // we do not hunt our own
					.filter(a => a != agent) // and we don't, for obvious reasons, hunt ourself

				// assume all animals nearby can be hunted for now
				val huntableAnimals = nearbyAnimals
				// pick the first one arbitrarily

				if (huntableAnimals.nonEmpty) {
					val bestHuntingTarget = huntableAnimals.head
					val animalEdibleParts = bestHuntingTarget.aux[AnimalData].parts
						.filter(part => eatingData.canEatEntity(part.entity))

					val expectedCalories = animalEdibleParts.fsum(p => p.entity.aux[FoodData].calories)
					// how many calories we expect to have to expend in order to catch our prey
					val expectedHuntCost = 0.0f

					if (expectedCalories > expectedHuntCost) {
						Some(new HuntAnimalGoal(bestHuntingTarget))
					} else {
						// if we're going to lose calories on the deal, don't bother
						None
					}
				} else {
					None
				}
			}
		} else {
			println("unhandled diet type")
			None
		}
	}

	def expectedPatchYield(agent: TAIAgent, AD: AnimalAIData, currentPatch : Patch, targetPatch: Patch): Float = {
		if (targetPatch.region.contains(agent.adjustedFootVoxelPos)) { -1000.0f }
		else {
			val FD = targetPatch.forage
			val curPatchPos = currentPatch.region.center
			val targetPatchPos = targetPatch.region.center
			// see what the pathing is like, or fall back on estimating by euclidean distance
			val expectedTravelDistance = currentPatch.pathCostTo.getOrElse(targetPatch,euclidDistance(curPatchPos,targetPatchPos))
			val travelSpeed = 1.5.m_s.inVoxelsPerSecond
			val travelTime = expectedTravelDistance / travelSpeed

			val movementMetabolicRate = agent.aux[MetabolicData].effectiveMetabolicRate(ActivityLevel.Light)
			val travelCost = travelTime * movementMetabolicRate
			val rawExpectedGain = FD.historicalForageCalories.getOrElse(defaultPatchExpectation)

			val timeSinceLastVisited = agent.world.currentTime - FD.lastForaged
			val timeWeight = powf((timeSinceLastVisited.inSeconds / patchCooldownTime(agent).inSeconds).clamp(0.0f,1.0f),2.0f)

			val timeWeightedExpectedGain = rawExpectedGain * timeWeight

			timeWeightedExpectedGain - travelCost
		}
	}

	def finishWithPatch (agent : TAIAgent, patch : Patch) {
		patch.forage.finishForageAndUpdateExpectedCalories(agent.world.currentTime)
	}


	override def isValidAgent(agent: TAIAgent): Boolean = super.isValidAgent (agent) && agent.hasAuxData[EatingData]

	override def split(agent: TAIAgent): SplitResult = this

	override def act(agent: TAIAgent, dt: UnitOfTime): AIResult = {
		// this operates purely through its prerequsite goals
		Success
	}
}

object ForageGoal {
	def canAgentEatCovering (canEatFlags : Set[TDescriptor], covering : Covering) : Boolean = {
		if (covering.notSentinel && !covering.stateContains(Covering.CroppedShort)) {
			covering.archetype match {
				case ps: PlantSpecies =>
					// is there a product this plant can make that can be eaten
					ps.products.exists(p => canEatFlags.exists(fd => fd.matches(p.archetype.exampleInstance)))
				case _ => false
			}
		} else { false }
	}
}






