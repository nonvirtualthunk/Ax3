package arx.axistential.game.logic.ai.goals

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/4/13
 * Time: 8:41 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.ai.AI.Reason.InsufficientItemsInInventory
import arx.axistential.ai.AI.Reason.UnexpectedState
import arx.axistential.ai._
import arx.axistential.game.archetypes.Material
import arx.axistential.game.archetypes.MaterialFlag
import arx.axistential.game.data.world.DesignationData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.entities.MaterialBlock
import arx.axistential.game.entities.helpers.VoxelConstructionSite
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.general.EntityLogic
import arx.axistential.game.logic.structuralsupport.TStructuralSupportLogic
import arx.axistential.modules.hunger.ActivityLevel
import arx.core.traits.TArxTraversable
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.voxelregions.VoxelRegion
import arx.tmp.game.logic.descriptors.TEntityDescriptor
import arx.requirements.NumberOfEntitiesRequirement

class BuildVoxelGoal(val voxels : VoxelRegion,val material : TEntityDescriptor) extends PhysicalGoal {
	override def activityLevel: ActivityLevel = ActivityLevel.Moderate
	import BuildVoxelGoal._

	var constructionSite : Option[VoxelConstructionSite] = None
	def withConstructionSite(cs:Option[VoxelConstructionSite]) = {
		constructionSite = cs
		this
	}

	def createPrerequisiteGoals(agent: TAIAgent): PrerequisiteGoalsResult = {
		MoveToRangeOfEffectGoal(agent,voxels.head) :: Nil
	}

	def fitness(agent: TAIAgent): Int = 0

	override def isReady (agent: TAIAgent): Boolean = {
		constructionSite match {
			case None => true
			case Some(cs) => {
				val (_,avail) = cs.inventory.entitiesToFulfillRequirement(new NumberOfEntitiesRequirement(material,voxels.size))
				avail >= voxels.size
			}
		}
	}

	/** If this goal can be split into more than one part, create two new goals, the first representing
	  * the split off portion, the second representing the remainder
 *
	  * @param agent the agent who is to perform the split off section
	  */
	def split(agent: TAIAgent): SplitResult = if ( constructionSite.isEmpty ) {
		val vcs = new VoxelConstructionSite(voxels.toList,material)
//		vcs.position = voxels.minBy(_.z).toObjectCoord
		agent.world.addEntity(vcs)

		val fetch = FetchRequirementsGoal(new NumberOfEntitiesRequirement(material, voxels.size), Some(vcs), false)
		val build = BuildVoxelGoal(voxels,material)
		build.constructionSite = Some(vcs)

		fetch -> build
	} else if ( voxels.size > 1 ) {
		divideVoxels(agent,1)
	} else {
		this
	}

	def divideVoxels(agent:TAIAgent,num:Int) = {
		// The above sorted is probably more efficient in terms of worker time, but it can lead to unbuildable sections
		// a more advanced heuristic that makes use of the number of adjacent support features or the like could be
		// a solution in the longer term
		// The downside of sorting purely by z is that it encourages building under ones own feet
//		val sorted = voxels.toList.sortBy( v => v.distanceTo(pe(agent).position).inVoxels )
		val sorted = voxels.toList.sortBy( v => v.z )
		val these = sorted.take(num)
		val those = sorted.drop(num)

		BuildVoxelGoal(these,material).withConstructionSite(constructionSite) ->
			BuildVoxelGoal(those,material).withConstructionSite(constructionSite)
	}

	def act(agent: TAIAgent, dt: UnitOfTime): AIResult = {
		progress += dt.inSeconds
		for ( cs <- constructionSite ) {
			voxels.foreachUnsafe(v => cs.pcntComplete += (v -> (progress / progressRequired(agent))))
		}
		if ( progress >= progressRequired(agent) ) {
			val r = req
			val sum = claimedEntities.fsum( r.amountSatisfiedBy )

			if ( sum < 1.0f ) { Noto.error("Progress on voxel build complete, but insufficient claimed entities to complete it : " + claimedEntities) }

			claimedEntities.head match {
				case mb : MaterialBlock => {
					val TD = agent.world.aux[TerrainData]
					TD.materialGrid.modificationBlock(voxels.head) {
						TD.setMaterialAt(voxels.head,mb.material.withFlag(MaterialFlag.Constructed))
					}
					val DD = agent.world.aux[DesignationData]
					voxels.foreachUnsafe( vox => DD.clearAdditionDesignation(vox.x,vox.y,vox.z) )
				}
				case _ => Noto.error("non mat block in build goal")
			}

			constructionSite match {
				case Some(cs) => {
					val claimedPhysEnts = claimedEntities.ofType[TPhysicalEntity]
					claimedPhysEnts.foreach(cs.inventory.removeHeldEntity)
					claimedPhysEnts.foreach(agent.world.removeEntity)
					val vset = voxels.toSet
					cs.voxels = cs.voxels.filterNot(vset.contains)
					if ( cs.voxels.isEmpty ) {
						EntityLogic.destroyEntity(cs)
					}
				}
				case None => Noto.error("Build Voxel Goal somehow completed with no construction site, that should be impossible")
			}

			AIResult.Success
		} else { AIResult.Continue }
	}
	def progressRequired(agent: TAIAgent): Float = 3

	def req = new NumberOfEntitiesRequirement(material,1)

	override def plan(agent: TAIAgent): AIResult = {
		if ( voxels.size == 1 ) {
			val voxel = voxels.head
			agent.world.aux[DesignationData].designateForImmediateAddition(voxel.x,voxel.y,voxel.z)

			constructionSite match {
				case Some(cs) => {

					val (entities,amount) =
						cs.inventory.entitiesToFulfillRequirement(req,None,agent,this)
					if ( amount < 1.0f ) {
						AIResult.Abort( InsufficientItemsInInventory(req,cs) )
					} else {
						val mat = entities.head.primaryMaterial
						if (SSL.hypotheticalSupportValueFor(voxels.head,mat)(agent.world).support < 1) {
							AIResult.Abort( NotSufficientlySupported(voxels.head,mat) )
						} else {
							AIResult.Success
						}
					}
				}
				case None => Noto.error("Invalid construction site state"); AIResult.Fail(UnexpectedState("Cons site"))
			}
		} else {
			AIResult.Success
		}
	}


	override def onAbort(agent: TAIAgent, reason : AIReason): Unit = {
		val DD = agent.world.aux[DesignationData]
		voxels.foreachUnsafe( vox => DD.designations.setIfEqual(vox.x,vox.y,vox.z,DD.immediateAdditionByte,DD.additionByte) )
	}

	override def onFail(agent : TAIAgent, reason : AIReason): Unit = {
		val DD = agent.world.aux[DesignationData]
		voxels.foreachUnsafe( vox => DD.clearAdditionDesignation(vox.x,vox.y,vox.z) )

		for ( cs <- constructionSite ) {
			val vset = voxels.toSet
			cs.voxels = cs.voxels.filterNot(vset.contains)
			if ( cs.voxels.isEmpty ) {
				EntityLogic.destroyEntity(cs)
			}
		}
	}

	override def onAdded(group: TAIGroup): Unit = {
		val DD = group.world.aux[DesignationData]
		voxels.foreachUnsafe( vox => DD.designateForAddition(vox.x,vox.y,vox.z) )
	}

	override def toString = s"BuildVoxelGoal(${voxels.toString()} out of $material)"
}
object BuildVoxelGoal {
	val SSL = pio[TStructuralSupportLogic]

	def apply (voxels : TArxTraversable[VoxelCoord],material : TEntityDescriptor) = voxels match {
		case vr : VoxelRegion => new BuildVoxelGoal(vr,material)
		case _ => new BuildVoxelGoal(VoxelRegion(voxels),material)
	}


	case class NotSufficientlySupported(v : VoxelCoord,addingMaterial : Material) extends AIReason {
		override def stillApplies(world: World, agent: TAIAgent): Boolean = {
			SSL.hypotheticalSupportValueFor(v,addingMaterial)(world).support < 1
		}
	}
}