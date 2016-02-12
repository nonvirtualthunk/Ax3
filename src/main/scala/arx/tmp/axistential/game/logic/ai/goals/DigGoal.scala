package arx.axistential.game.logic.ai.goals

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/8/13
 * Time: 11:52 AM
 */
import arx.axistential.ai.AI.Reason.InaccessibleVoxels
import arx.axistential.ai.AI.Reason.UnexpectedState
import arx.axistential.ai.SplitResult.Split
import arx.axistential.ai._
import arx.axistential.allCardinalAdjacents
import arx.axistential.game.archetypes.Material
import arx.axistential.game.data.entity.InventoryData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.modules.hunger.ActivityLevel
import arx.core.datastructures.OneOrMore
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.VoxelCoord

case class DigGoal(voxels : OneOrMore[VoxelCoord]) extends PhysicalGoal {
	override def activityLevel: ActivityLevel = ActivityLevel.Heavy

	def createPrerequisiteGoals(agent: TAIAgent) = List(
		MoveToRangeOfEffectGoal(agent.asInstanceOf[TPhysicalEntity],voxels.head)
	)

//	def fitness(agent: TAIAgent): Int = agent match {
//		case pe : TPhysicalEntity => pe.position.distanceTo(voxels.head)
//	}
	val fitnessByDistance = List(
		0.0f -> AI.Fitness.VeryFit,15.0f -> AI.Fitness.Fit,30.0f -> AI.Fitness.SomewhatFit,45.0f -> AI.Fitness.Normal,
		60.0f -> AI.Fitness.SomewhatUnfit,75.0f -> AI.Fitness.Unfit,100.0f -> AI.Fitness.VeryUnfit
	)

	def fitness(agent:TAIAgent) = agent match {
		case pe : TPhysicalEntity => linInterpolatei(pe.position.distanceTo(voxels.head).inVoxels,fitnessByDistance).toInt
		case _ => 0
	}


	def isAccessible ( terrain : TerrainData, v : VoxelCoord ) = {
		allCardinalAdjacents(v).exists( a => ! terrain.isSolid(a) )
	}
	/** If this goal can be split into more than one part, create two new goals, the first representing
	  * the split off portion, the second representing the remainder
	  * @param agent the agent who is to perform the split off section
	  */
	def split(agent: TAIAgent) = {
		val terrain = agent.world.aux[TerrainData]

		voxels.toList match {
			case Nil => SplitResult.None(UnexpectedState("no voxels"))
			case only :: Nil => {
				if ( isAccessible(terrain,only) ) {
					this
				} else {
					InaccessibleVoxels(only)
				}
			}
			case head :: more => {
				voxels.find( v => isAccessible(terrain,v) ) match {
					case None => SplitResult.None( InaccessibleVoxels(voxels) )
					case Some(v) => {
						Split(DigGoal(v) , DigGoal(voxels without v))
					}
				}
			}
		}
	}

	def act(agent: TAIAgent, dt : UnitOfTime): AIResult = {
		progress += dt.inSeconds * 60.0f
		if ( progress < progressRequired(agent) ) {
			AIResult.Continue
		} else {
			val TD = agent.world.aux[TerrainData]
			if ( voxels.forall( v => TD.materialByteAt(v) == 0 ) ) {
				AIResult.Fail( UnexpectedState("no voxels to dig") )
			} else {
				val preDigMaterials = voxels.toList.map( v => TD.materialAt(v) )
				TD.materialGrid.modificationBlock(voxels.toList) {
					voxels.foreach( v => TD.setMaterialAt(v,Material.Sentinel) )
				}

				for ( (mat,vpos) <- preDigMaterials.zip(voxels.toList) if mat.notSentinel ) {
					if ( rand(0.0f,1.0f) < 0.25f ) { //give a block a quarter of the time
						val block = mat.withoutFlags.createInstance
						if ( ! agent.aux[InventoryData].holdEntityIfPossible(block) ) {
							block.position = vpos.toObjectCoord
							agent.world.addEntity(block)
						}
					}
				}

				AIResult.Success
			}
		}
	}

	def progressRequired(agent: TAIAgent): Float = {
		30
	}
}


