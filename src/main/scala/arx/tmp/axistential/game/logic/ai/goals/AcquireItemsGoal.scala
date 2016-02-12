package arx.axistential.game.logic.ai.goals

/**
 */

import arx.Prelude._
import arx.axistential.ai.AI.Reason.InvalidAgent
import arx.axistential.ai.AI.Reason.MissingItems
import arx.axistential.ai.AI.Reason.UnexpectedState
import arx.axistential.ai._
import arx.axistential.game.data.entity.ClaimData
import arx.axistential.game.data.entity.InventoryData
import arx.axistential.game.data.entity.NoClaimData
import arx.axistential.game.data.entity.NoInventory
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.modules.hunger.ActivityLevel
import arx.core.datastructures.OneOrMore
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.entities.core.GameEntity
import arx.requirements.TRequirement

case class AcquireItemsGoal(items : OneOrMore[TPhysicalEntity], fromEntity : TPhysicalEntity) extends PhysicalGoal {
	override def activityLevel: ActivityLevel = ActivityLevel.Light
	def createPrerequisiteGoals(agent: TAIAgent) = agent match {
		case pe : TPhysicalEntity => MoveToRangeOfEffectGoal(pe,fromEntity) :: Nil
		case _ => InvalidAgent(agent)
	}

	def fitness(agent: TAIAgent): Int = 0

	/** If this goal can be split into more than one part, create two new goals, the first representing
	  * the split off portion, the second representing the remainder
	  * @param agent the agent who is to perform the split off section
	  */
	def split(agent: TAIAgent): SplitResult = this

	def act(agent: TAIAgent, dt : UnitOfTime): AIResult = {
		progress += 1
		if ( progress > progressRequired(agent) ) {
			val fromID = fromEntity.aux[InventoryData]
			val toID = agent.aux[InventoryData]
			for ( item <- items ) {
				fromID.removeHeldEntity(item)
				toID.holdEntityForced(item)
			}
			AIResult.Success
		} else { AIResult.Continue }
	}

	def progressRequired(agent: TAIAgent): Float = 30
}

/**
 * Acquire entities that fulfill the given requirements, and transport them to the given entity (if given) or simply
 * to the agent (if not).
 */
case class FetchRequirementsGoal ( requirements : OneOrMore[TRequirement], toEntity : Option[TPhysicalEntity], partialSuccessAllowed : Boolean ) extends Goal {
	def createPrerequisiteGoals(agent: TAIAgent): PrerequisiteGoalsResult = {
		// if there is no actual requirement that needs to be fulfilled, just drop right out, nothing to do here
		if (requirements.forall(_.amount <= 0.0f)) { return Nil }

		var stockpiles = agent.aiGroup.entities.ofType[TPhysicalEntity].filter( e => e.auxDataOrElse[InventoryData](NoInventory).isSourceInventory )
		agent match {
			case pe : TPhysicalEntity => stockpiles += pe //The agent is a valid source for entities, assuming they are not otherwise claimed
			case _ =>
		}
		toEntity match {
			case Some(destEntity) => stockpiles += destEntity //if we're taking it somewhere, we should consider things that are already there
			case _ =>
		}

		if ( stockpiles.isEmpty ) { UnexpectedState("no stockpiles") }
		else {
			var satisfiedByRequirement = requirements.map(r => r -> 0.0f).toMap
			var remainingRequirements = requirements.toSet
			var ret = List[Goal]()
			var entitiesByStockpile = Map[TPhysicalEntity,Traversable[TPhysicalEntity]]()
			var allEntities = List[TPhysicalEntity]()
			var allEntitiesNotAtDest = List[TPhysicalEntity]()

			var referencePoint = agent match { case tpe : TPhysicalEntity => tpe.position ; case _ => VoxelCoord.Sentinel }
			var closestStockpile = stockpiles.minBy( _.position.distanceTo(referencePoint) )
			while ( remainingRequirements.nonEmpty && stockpiles.nonEmpty ) {
				val closestIsDest = closestStockpile == toEntity.getOrElse(GameEntity.Sentinel)
				stockpiles = stockpiles - closestStockpile

				var allEntitiesFromHere = Set[TPhysicalEntity]()
				for ( req <- remainingRequirements ; alreadySatisfied = satisfiedByRequirement(req) ) {
					val (entities,deltaSatisfied) = closestStockpile.aux[InventoryData].entitiesToFulfillRequirement(req,Some( req.amount - alreadySatisfied ),agent,this)
					allEntitiesFromHere ++= entities
					allEntities :::= entities.toList
					if (!closestIsDest) { allEntitiesNotAtDest :::= entities.toList }
					satisfiedByRequirement += req -> (alreadySatisfied + deltaSatisfied)
				}
				remainingRequirements = remainingRequirements.filter( req => req.amount > satisfiedByRequirement(req) )

				entitiesByStockpile += closestStockpile -> allEntitiesFromHere

				if ( allEntitiesFromHere.nonEmpty ) {
					if ( !closestIsDest && closestStockpile != agent ) {
						ret :+= AcquireItemsGoal(allEntitiesFromHere,closestStockpile)
					}
					referencePoint = closestStockpile.position
				}
				if ( stockpiles.nonEmpty ) {
					closestStockpile = stockpiles.minBy( _.position.distanceTo(referencePoint) )
				}
			}

			// This is a failure if we can't find everything and don't allow partial successes, or we do but have found
			// nothing at all
			if ( (remainingRequirements.nonEmpty && ! partialSuccessAllowed) || allEntities.isEmpty ) {
				UnexpectedState("could not acquire all") //Could not acquire all, so don't bother, our assumption here is
													// that the general machinery will clear out all the claims and associated state
			} else {
				if (allEntitiesNotAtDest.isEmpty) {
					// no action required
					Nil
				} else {
					ret ::: toEntity.map( destEnt => DeliverItemsGoal(allEntitiesNotAtDest,destEnt) ).toList
				}
			}
		}
	}

	def fitness(agent: TAIAgent): Int = 0

	/** If this goal can be split into more than one part, create two new goals, the first representing
	  * the split off portion, the second representing the remainder
	  * @param agent the agent who is to perform the split off section
	  */
	def split(agent: TAIAgent): SplitResult = this

	def act(agent: TAIAgent, dt : UnitOfTime): AIResult = AIResult.Success //this has no action itself, it simply delegates

	def progressRequired(agent: TAIAgent): Float = 0
}

case class DeliverItemsGoal(items : OneOrMore[TPhysicalEntity], toEntity : TPhysicalEntity) extends PhysicalGoal {
	override def activityLevel: ActivityLevel = ActivityLevel.Light
	def createPrerequisiteGoals(agent: TAIAgent) = agent match {
		case pe : TPhysicalEntity => MoveToRangeOfEffectGoal(pe,toEntity) :: Nil
		case _ => Nil
	}

	def fitness(agent: TAIAgent): Int = 0

	def split(agent: TAIAgent): SplitResult = this

	def act(agent: TAIAgent, dt : UnitOfTime): AIResult = {
		val inventory = agent.auxDataOrElse[InventoryData](NoInventory)
		if ( items.forall( inventory.containsEntity ) ) {
			toEntity.auxDataOpt[InventoryData] match {
				case Some(targetInventory) => {
					val storeResults = items.map( item =>
						if ( targetInventory.holdEntityIfPossible(item) ) { inventory.removeHeldEntity(item) ; true }
						else { false }
					)

					if ( storeResults.forall( identity ) ) { //All successes
						AIResult.Success
					} else {
						AIResult.Fail( UnexpectedState("no space despite reservation") ) //This is unexpected, because space should have been reserved already
					}
				}
				case None => AIResult.Fail( UnexpectedState("no inventory data") )
			}
		} else {
			AIResult.Fail( MissingItems(agent,items) )
		}
	}

	def progressRequired(agent: TAIAgent): Float = 0

	override def toString: String = s"${this.getClass.getSimpleName}(${items.toShortString} to $toEntity)"
}

class EmptyInventoryGoal extends Goal with PassiveGoal {
	val prioritiesByPercent = List( 0.0f -> AI.Priority.Minimum , 0.5f -> AI.Priority.Low , 0.8f -> AI.Priority.Normal , 1.0f -> AI.Priority.MidHigh )

	override def priority(agent: TAIAgent): Int = {
		val ID = agent.aux[InventoryData]
		val limit = ID.storageLimit.getOrElse(1000)
		val current = ID.allHeldEntities.count( _.auxDataOrElse[ClaimData](NoClaimData).notClaimed )
		val percent = current / math.max(limit,1).toFloat

		linInterpolatei(percent,prioritiesByPercent).toInt
	}

	def createPrerequisiteGoals(agent: TAIAgent) = {
		agent.auxDataOpt[InventoryData] match {
			case Some(inventory) => {
				val unclaimedEntities = inventory synchronized {
					val cur = inventory.allHeldEntities.filter( _.auxDataOrElse[ClaimData](NoClaimData).notClaimed )
					cur.foreach( _.auxData[ClaimData].claim(agent,this))
					cur
				}

				var stockpiles = agent.aiGroup.entities.ofType[TPhysicalEntity].filter( e => e.auxDataOrElse[InventoryData](NoInventory).isSourceInventory )

				if ( stockpiles.isEmpty ) { UnexpectedState("no stockpiles") }
				else {
					var remainingEntities = unclaimedEntities
					var ret = List[Goal]()
					var entitiesByStockpile = Map[TPhysicalEntity,Traversable[TPhysicalEntity]]()

					var referencePoint = agent match { case tpe : TPhysicalEntity => tpe.position ; case _ => VoxelCoord.Sentinel }
					var closestStockpile = stockpiles.minBy( _.position.distanceTo(referencePoint) )
					while ( remainingEntities.nonEmpty && stockpiles.nonEmpty ) {
						stockpiles = stockpiles - closestStockpile
						val (reservedEntities,unreservedEntities) =
							remainingEntities.partition( e => closestStockpile.aux[InventoryData].reserveSpaceForEntityIfPossible(e) )
						entitiesByStockpile += closestStockpile -> reservedEntities

						remainingEntities = unreservedEntities
						if ( reservedEntities.nonEmpty ) {
							ret :+= DeliverItemsGoal(reservedEntities,closestStockpile)
							referencePoint = closestStockpile.position
						}
						if ( stockpiles.nonEmpty ) {
							closestStockpile = stockpiles.minBy( _.position.distanceTo(referencePoint) )
						}
					}

					if ( remainingEntities.nonEmpty ) {
						for ( (stockpile,entities) <- entitiesByStockpile ; entity <- entities ) {
							stockpile.aux[InventoryData].removeReservationForEntity(entity)
						}
						UnexpectedState("some entities could not be stored")
					} else {
						ret
					}
				}
			}
			case None => Nil
		}
	}

	/** If this goal can be split into more than one part, create two new goals, the first representing
	  * the split off portion, the second representing the remainder
	  * @param agent the agent who is to perform the split off section
	  */
	def split(agent: TAIAgent): SplitResult = this

	def act(agent: TAIAgent, dt : UnitOfTime): AIResult = {
		AIResult.Success
	}
}
