package arx.axistential.game.components

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/11/13
 * Time: 2:38 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.axistential.game.data.entity.AIData
import arx.axistential.game.entities.CreatureEntity
import arx.axistential.game.entities.CreatureStateData
import arx.axistential.game.events.CreaturesLeftSightEvent
import arx.axistential.game.events.CreaturesSightedEvent
import arx.axistential.game.events.EnemiesLeftSightEvent
import arx.axistential.game.events.EnemiesSightedEvent
import arx.axistential.game.logic.ai.goals.MoveGait
import arx.axistential.game.logic.general.FriendOrFoeLogic
import arx.axistential.game.logic.general.FriendOrFoeLogic._
import arx.axistential.game.world.AxistentialWorld.AxistentialWorld
import arx.core.units.Moment
import arx.core.units.UnitOfDistance
import arx.core.units.UnitOfTime

class EnemyAwarenessComponent extends GameEngineComponent with AtIntervalGameEngineComponent {
	lazy val consciousEntities = world.createEntityTypeQuery[ CreatureEntity ]

	def interval = 1.second
	def onInterval() {
		for ( entity <- consciousEntities ; aiData = entity.aux[AIData] ) {
			var detectedCreatures = Set[CreatureEntity]()
			for ( otherEntity <- consciousEntities if otherEntity.alive && otherEntity != entity ) {
				val maxObservationDistance = math.max(entity.sightRange.inVoxels, entity.hearingRange.inVoxels)
				val dist = distance(otherEntity.position,entity.position)

				if ( dist <= maxObservationDistance ) {
					// If the other entity is stealthy and trying to sneak we don't automatically add them
					if (otherEntity.stealthActive && (otherEntity.baseAudioStealth > 0.0f || otherEntity.baseVisualStealth > 0.0f)) {
						// But if they've already been detected, we assume they remain detected until they move out of range
						if (aiData.detectedCreatures.contains(otherEntity)) {
							detectedCreatures += otherEntity
						} else {
							val gait = otherEntity.aux[CreatureStateData].currentMovementGait
							// Otherwise, we calculate the likelihood of detection, then roll to determine if detection happens
							val audioChance = detectionChance(dist,entity.hearingRange,entity.hearingPrecision,otherEntity.baseAudioStealth,gait,interval)
							val visualChance = detectionChance(dist,entity.sightRange,entity.sightPrecision,otherEntity.baseVisualStealth,gait,interval)
	
							if (randomGenerator.nextFloat() < audioChance || randomGenerator.nextFloat() < visualChance) {
//								Noto.finest(AILogging,s"${entity.name} detected ${otherEntity.name}, chances were $audioChance, $visualChance")
								detectedCreatures += otherEntity
							}
						}
					} else {
//						Noto.finest(AILogging,s"${entity.name} detected ${otherEntity.name}, was not stealthy")
						detectedCreatures += otherEntity
					}
				}
			}
			
			val enteredDetection = detectedCreatures diff aiData.detectedCreatures
			val exitedDetection = aiData.detectedCreatures diff detectedCreatures
			
			aiData.detectedCreatures = detectedCreatures

			if ( enteredDetection.nonEmpty ) {
				val enemiesEnteredSight = enteredDetection.filter(_.isThreatTo(entity))
				entity.eventHappened( CreaturesSightedEvent(entity,enteredDetection) )
				if (enemiesEnteredSight.nonEmpty) {
					// Once an enemy is in sight, the memory is no longer pertinent
					for (enemy <- enemiesEnteredSight) {
						aiData.removeEnemyMemory(enemy)
					}
					entity.eventHappened( EnemiesSightedEvent(entity,enemiesEnteredSight) )
				}
			}

			if ( exitedDetection.nonEmpty ) {
				val enemiesLeft = exitedDetection.filter(_.isThreatTo(entity))
				entity.eventHappened( CreaturesLeftSightEvent(entity,exitedDetection) )
				if (enemiesLeft.nonEmpty) {
					// As the enemy leaves sight, remember its last location for future reference
					for (enemy <- enemiesLeft) {
						aiData.addEnemyMemory(AIData.EnemyMemory(enemy,enemy.position,world.time))
					}
					entity.eventHappened( EnemiesLeftSightEvent(entity,enemiesLeft) )
				}
			}
		}
	}

	def detectionChance (dist : Float, maxDist : UnitOfDistance, precision : Float, stealth : Float, gait : MoveGait, interval : UnitOfTime) = {
		val invDistPercent = clamp(1.0f - dist / maxDist.inVoxels,0.0f,1.0f)
		val strength = invDistPercent * precision

		// We express in terms of the chance of being detected from 50% distance, so we want to scale to that
		val adjustedStrength = strength * 2.0f

		// Raw chance is per moment
		val rawChance = (1.0f - stealth) * interval.in(Moment)

		// How much the chance should be changed by the current movement speed of the creature, still things are much harder to detect
		// with hearing or sight at least. Currently our formula is to use half the effect it has on speed. So if the speed multiplier
		// is 0.5f, we multiply the detection chance by 0.75f, if the speed mult is 2.0f, we multiply detection by 1.5f
		val gaitAdjustment = 1.0f + (gait.speedMultiplier - 1.0f) * 0.5f
		
		rawChance * adjustedStrength * gaitAdjustment
	}
}