package arx.axistential.game.data.entity

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/16/13
 * Time: 3:40 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.axistential.ai.Goal
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.core.datastructures.MultiMap
import arx.core.traits.TSentinel
import arx.core.traits.TSentinelable
import arx.tmp.game.logic.entities.core._
import arx.tmp.game.logic.entities.data.TGameEntityAuxData
import arx.requirements.TRequirement

import scala.collection.mutable

class InventoryData extends TGameEntityAuxData with TSentinelable {
	protected var forEntity : TPhysicalEntity = TPhysicalEntity.Sentinel
	protected val reservations = new mutable.HashSet[TPhysicalEntity]()
	protected val entities = new MultiMap[GameArchetype,TPhysicalEntity]()
	protected var totalStoredEntities = 0

	/**
	 * Determines whether this inventory should be used as a general source of entities
	 * when requirements need to be filled. This should generally be false, except in the
	 * case of stockpiles and entities that fulfill similar purposes.
	 */
	var isSourceInventory : Boolean = false

	/**
	 * Determines the soft-maximum number of entities that can be stored in this inventory.
	 * If <code>None</code>, there is no limit at all. If provided, this inventory will discourage
	 * further storage, but will not necessarily prevent it, depending on the situation.
	 */
	var storageLimit : Option[Int] = Some(10)

	def containsEntity ( entity : TPhysicalEntity ) = synchronized {
		entities.get(entity.archetype).contains(entity)
	}

	/**
	 * All entities held by this inventory at this time.
 *
	 * @return
	 */
	def allHeldEntities : Traversable[TPhysicalEntity] = synchronized { entities.flattenedValues }

	/**
	 * Returns any available, held entities that can be used to fulfill the given requirement
	 * up to the point that they would totally satisfy the requirement. Entities that are already
	 * claimed will not be returned. This function will <strong>not</strong> claim entities,
	 * for that, use the variation of this function that takes more arguments.
	 *
	 * This function is synchronized, so its results should be internally consistent, though
	 * they are not guaranteed to remain so after the call returns.
 *
 	 * @param requirement the requirement to find matching entities for
	 * @param amountLimit the maximum amount to be collected towards the requirement, or <code>None</code> if
	 *                    the total amount the requirement can use should be collected
	 * @return the relevant entities, if any, and the amount that they satisfy, between them
	 */
	def entitiesToFulfillRequirement ( requirement : TRequirement , amountLimit : Option[Float] = None ) : (Traversable[TPhysicalEntity],Float) = {
		entitiesToFulfillRequirement(requirement,amountLimit,null,null)
	}

	/**
	 * Claims and returns any available, held entities that can be used to fulfill the given requirement
	 * up to the point that they would totally satisfy it. Entities that are already
	 * claimed will not be returned.
	 *
	 * This function is synchronized, so its results should be internally consistent, though
	 * they are not guaranteed to remain so after the call returns.
 *
	 * @param requirement the requirement to find matching entities for
	 * @param amountLimit the maximum amount to be collected towards the requirement, or <code>None</code> if
	 *                    the total amount the requirement can use should be collected
	 * @param claimForEnt the entity that is claiming the matching contents of this inventory
	 * @param claimForGoal the goal for which the entity is doing the claiming
	 * @return the relevant entities, if any, and the amount that they satisfy, between them
	 */
	def entitiesToFulfillRequirement ( requirement : TRequirement, amountLimit : Option[Float], claimForEnt : GameEntity , claimForGoal : Goal ) : (Traversable[TPhysicalEntity],Float) = synchronized {
		val limit = amountLimit.getOrElse( requirement.amount )

		var sum = 0.0f
		var ret = List[TPhysicalEntity]()

		val iter = entities.flattenedValuesIterator
		while ( iter.hasNext && sum < limit ) {
			val ent = iter.next()
			if ( ent.auxDataOrElse[ClaimData](NoClaimData).notClaimed ) {
				var amountSatisfied = requirement.amountSatisfiedBy(ent)
				if ( amountSatisfied > 0 ) {
					ret ::= ent
					sum += amountSatisfied
					if ( claimForEnt != null ) {
						ent.aux[ClaimData].claim(claimForEnt,claimForGoal)
					}
				}
			}
		}
		ret -> sum
	}

	/**
	 * Checks whether or not the entities in this inventory can be used to fully satisfy the provided requirement.
	 * 
	 * @return <code>true</code> if there are entities sufficient to fulfill the requirement, <code>false</code> otherwise
	 */
	def couldFulfillRequirement (requirement : TRequirement) = entitiesToFulfillRequirement(requirement)._2 >= requirement.amount

	/**
	 * Attempt to hold the provided entity in this inventory. If successful, the given
	 * entity will be added to the list of held entities and <code>true</code> will be
	 * returned, otherwise the inventory will not be modified, and <code>false</code>
	 * will be returned.
	 *
	 * This function is synchronized, so its results should be internally consistent.
 *
	 * @param ent the entity to hold
	 * @return success
	 */
	def holdEntityIfPossible ( ent : TPhysicalEntity ) : Boolean = synchronized {
		if ( spaceAvailableForEntity(ent) ) {
//			val cur = entities.get(ent.archetype)
//			if ( cur.isEmpty ) {
			subHoldEntity(ent)
//			} else {
//				val head = cur.head
//				head match {
//					case stackable : TStackableEntity =>
//						entities.removeAll(ent.archetype)
//						entities.add(ent.archetype,new ItemStack(head,))
//					case stack : ItemStack => stack.count += 1
//					case _ => entities.add(ent.archetype,ent)
//				}
//			}
			true
		} else { false }
	}

	/**
	 * Hold the given entity, regardless of space and reservation considerations.
 *
	 * @param ent the entity to hold
	 * @return true if there was space, false if it had to overrule space restrictions
	 */
	def holdEntityForced ( ent : TPhysicalEntity ) : Boolean = synchronized {
		val ret = spaceAvailableForEntity(ent)
		subHoldEntity(ent)
		ret
	}

	protected def subHoldEntity ( ent : TPhysicalEntity) {
		ent.heldBy = Some(forEntity)
		entities.add(ent.archetype,ent)
		removeReservationForEntity(ent)
	}

	def removeHeldEntity ( ent : TPhysicalEntity ) : Boolean = synchronized {
		entities.remove(ent.archetype,ent)
	}

	def reserveSpaceForEntityIfPossible ( ent : TPhysicalEntity ) : Boolean = synchronized {
		if ( spaceAvailableForEntity(ent) ) {
			reservations.add(ent)
			true
		} else { false }
	}
	def removeReservationForEntity ( ent : TPhysicalEntity ) = synchronized {
		reservations.remove(ent)
	}
	def spaceAvailableForEntity ( ent : TPhysicalEntity ) : Boolean = synchronized {
		if ( reservations.contains(ent) ) {  //If space has been reserved, we're committed
			true
		} else {
			storageLimit match {
				case Some(sl) => sl > reservations.size + totalStoredEntities
				case _ => true
			}
		}
	}


	override def onAssignedToEntity(entity: TGameBase): Unit = forEntity = entity match {
		case pe : TPhysicalEntity => pe
		case _ => forEntity
	}


}
object InventoryData {
	val Sentinel : InventoryData = NoInventory
}

object NoInventory extends InventoryData with TSentinel {
	storageLimit = Some(0)
	override def allHeldEntities: Traversable[TPhysicalEntity] = Nil
	override def entitiesToFulfillRequirement(requirement: TRequirement, amountLimit : Option[Float]): (Traversable[TPhysicalEntity], Float) = Nil -> 0.0f
	override def entitiesToFulfillRequirement(requirement: TRequirement, amountLimit : Option[Float], claimForEnt: GameEntity, claimForGoal: Goal): (Traversable[TPhysicalEntity], Float) = Nil -> 0.0f
	override def holdEntityIfPossible(ent: TPhysicalEntity): Boolean = false
	override def reserveSpaceForEntityIfPossible(ent: TPhysicalEntity): Boolean = false
	override def removeReservationForEntity(ent: TPhysicalEntity): Boolean = false
	override def spaceAvailableForEntity(ent: TPhysicalEntity): Boolean = false
}