package arx.axistential.game.logic.general

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/11/13
 * Time: 2:46 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.axistential.game.archetypes.species.Species
import arx.axistential.game.data.entity.animal.EatingData
import arx.axistential.game.entities.CreatureEntity

import scala.collection.mutable


object FriendOrFoeLogic {
	val cachedRelationships = new mutable.HashMap[(Species,Species),FriendOrFoeResponse]

	def relationTo ( creature : CreatureEntity, otherCreature : CreatureEntity ) = {
		if ( creature.aiGroup == otherCreature.aiGroup && creature.aiGroup.notSentinel ) {
			Friend
		} else {
			cachedRelationships.getOrElseUpdate((creature.species,otherCreature.species),computeRelationship(creature,otherCreature))
		}
	}

	protected def computeRelationship(creature : CreatureEntity, otherCreature : CreatureEntity) = {
		val creatureED = creature.aux[EatingData]
		val otherED = otherCreature.aux[EatingData]

		// TODO: Adds checking for comparative size, no need for a horse to run away from
		// a hawk, for example
		val canEatOther = creatureED.dietType.eatsLiveAnimals
		val otherCanEat = otherED.dietType.eatsLiveAnimals

		if (canEatOther && ! otherCanEat) {
			PredatorOf
		} else if (!canEatOther && otherCanEat) {
			PreyOf
		} else if (canEatOther && otherCanEat) {
			// could maybe add a special case for mutual predation
			PredatorOf
		} else {
			Neutral
		}
	}

	implicit class CreatureWrapper ( val creature : CreatureEntity ) extends AnyVal {
		def relationTo ( otherCreature : CreatureEntity ) : FriendOrFoeResponse = {
			FriendOrFoeLogic.relationTo(creature,otherCreature)
		}
		def isEnemyOf ( otherCreature : CreatureEntity ) = relationTo(otherCreature).isEnemy
		def isFriendOf ( otherCreature : CreatureEntity ) = relationTo(otherCreature).isFriend
		def isThreatTo ( otherCreature : CreatureEntity ) = relationTo(otherCreature).isThreatTo
	}

	class FriendOrFoeResponse (val isFriend:Boolean,val isEnemy:Boolean, val isThreatTo : Boolean)
	case object Friend extends FriendOrFoeResponse(true,false,false)
	case object Neutral extends FriendOrFoeResponse(false,false,false)
	case object Enemy extends FriendOrFoeResponse(false,true,true)
	case object PredatorOf extends FriendOrFoeResponse(false,true,true)
	case object PreyOf extends FriendOrFoeResponse(false,true,false)

}
