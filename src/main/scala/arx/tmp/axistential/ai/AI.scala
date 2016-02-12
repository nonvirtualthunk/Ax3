package arx.axistential.ai

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/11/13
 * Time: 3:09 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.axistential.game.data.entity.InventoryData
import arx.axistential.game.data.entity.NoInventory
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.general.PhysicalEntityLogic
import arx.core.datastructures.OneOrMore
import arx.core.units.UnitOfDistance
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.TMajorCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.voxelregions.VoxelRegion
import arx.tmp.game.logic.entities.core.GameEntity
import arx.requirements.TRequirement

object AI {
	object Priority {
		val Maximum = 100
		val AvoidingDeath = 80
		val LifeOrDeath = 70
		val ImminentDanger = 60
		val High = 50
		val MidHigh = 25
		val Normal = 0
		val MidLow = -25
		val Low = -50
		val Unimportant = -75
		val Minimum = -100
	}

	object Fitness {
		val Maximum = 100
		val VeryFit = 75
		val Fit = 50
		val SomewhatFit = 25
		val Normal = 0
		val SomewhatUnfit = -25
		val Unfit = -50
		val VeryUnfit = -75
		val Minimum = -100
	}

	object Reason {
//		def AgentInInvalidPosition( agent : TAIAgent with TPhysicalEntity , desiredPosition : TMajorCoord, actualPosition : TMajorCoord ) = new AgentInInvalidPosition(agent,desiredPosition,actualPosition)
		case class AgentInInvalidPosition ( agent : TAIAgent with TPhysicalEntity , desiredPosition : TMajorCoord, actualPosition : TMajorCoord ) extends AIReason {
			def stillApplies(world: World, specificAgent: TAIAgent) = {
				// TODO: Should we modify this to only apply when agent == specificAgent?
				agent.position.toVoxelCoord != desiredPosition.toVoxelCoord
			}
		}

		case class InvalidAgent ( agent : TAIAgent , reason : String = "" ) extends AIReason {
			def stillApplies(world: World, agent: TAIAgent) = true
		}
		case class InvalidTarget ( target : GameEntity , reason : String = "" ) extends AIReason {
			def stillApplies(world: World, agent: TAIAgent) = true
		}

		case class AgentNotOnSolidGround ( agent : TPhysicalEntity ) extends AIReason {
			def stillApplies(world: World, specificAgent: TAIAgent): Boolean = {
				val terrain = world.aux[TerrainData]
				agent.collisionShape.intersect(-0.1f,2,terrain.materialGrid) < 0.0f
			}
		}

		case class InaccessibleVoxels(locations:OneOrMore[VoxelCoord]) extends AIReason {
			def stillApplies(world: World, agent: TAIAgent): Boolean = {
				locations.forall( v => world.aux[TerrainData].isSolid( v.plusZ(1) ) )
			}
		}

		case object IsSentinelReason extends AIReason {
			override def stillApplies(world: World, agent: TAIAgent): Boolean = true
		}

		case class MissingItems ( agent : TAIAgent, items : OneOrMore[TPhysicalEntity] ) extends AIReason {
			def stillApplies(world: World, agent: TAIAgent): Boolean = {
				// This likely isn't going to self correct so we should probably toss off this reason after a few seconds
				if (agent.worldTime - timestamp > 5.seconds) {
					true
				} else {
					val inventory = agent.auxDataOrElse[InventoryData](NoInventory)
					! items.forall( inventory.containsEntity )
				}
			}
		}

		case class InsufficientItemsInInventory ( items : TRequirement , inventoryEntity : GameEntity ) extends AIReason {
			def stillApplies(world: World, agent: TAIAgent): Boolean = {
				if (agent.worldTime - timestamp > 5.seconds) {
					true
				} else {
					val inventory = inventoryEntity.aux[InventoryData]
					val (_,avail) = inventory.entitiesToFulfillRequirement(items)
					avail < items.amount
				}
			}
		}

		case class NoPath(from : VoxelCoord, destination:VoxelRegion, terrainRevisions : Map[VoxelCoord,Int]) extends AIReason {
			var lastChecked = timestamp
			def stillApplies(world: World, agent: TAIAgent): Boolean = {
				// if we've passed twenty seconds, give it a go, regardless
				if (agent.worldTime - timestamp > 20.seconds) {
					false
					// if it's been less than 4 seconds since we last checked the modifications, don't bother
				} else if (agent.worldTime - lastChecked < 4.seconds) {
					true
				} else {
					// otherwise, see if any of our relevant Taleae have been changed (this will generally just be
					// the start and end taleae, so it's not going to be perfect, but should be better than nothing)
					val TD = world.aux[TerrainData]
					if (terrainRevisions.exists {
						case (k,v) => TD.materialGrid.taleaFor(k.x,k.y,k.z).modifiedCount > v
					}) {
						false
					} else {
						lastChecked = agent.worldTime
						true
					}
				}
			}
		}

		case class NoPathToMovingTarget (from : VoxelCoord, destination:TPhysicalEntity) extends AIReason {
			val originated = timestamp
			val startPos = destination.position.toVoxelCoord
			def stillApplies(world: World, agent: TAIAgent): Boolean = {
				agent.worldTime - originated < 1.moment && destination.position.toVoxelCoord == startPos
			}
		}

		object OutdatedPlan extends AIReason {
			override def stillApplies(world: World, agent: TAIAgent): Boolean = false

			override def toString: String = "Outdated Plan"
		}

		case object Stuck extends AIReason {
			def stillApplies(world: World, agent: TAIAgent): Boolean = agent.worldTime - timestamp < 1.seconds
		}

		case class UnexpectedState(message:String) extends AIReason {
			def stillApplies(world: World, agent: TAIAgent): Boolean = agent.worldTime - timestamp < 5.seconds
		}

		case object Unfinished extends AIReason{
			def stillApplies(world: World, agent: TAIAgent): Boolean = true
		}

		case class NoValidTarget(cooldown : UnitOfTime) extends AIReason {
			def stillApplies(world: World, agent: TAIAgent) = {
				world.time - timestamp < cooldown
			}
		}

		case class OutOfRange (from : TPhysicalEntity, to : TPhysicalEntity, maxRange : UnitOfDistance) extends AIReason {
			override def stillApplies(world: World, agent: TAIAgent): Boolean = {
				PhysicalEntityLogic.minimumDistanceBetweenEntities(from,to) > maxRange
			}
		}

		case object Recalculating extends AIReason {
			override def stillApplies(world: World, agent: TAIAgent): Boolean = true
		}
	}
}
