package arx.axistential.game.logic.ai.goals.animals

import arx.application.Noto
import arx.axistential.ai.AI.Reason.UnexpectedState
import arx.axistential.ai._
import arx.axistential.game.archetypes.Covering
import arx.axistential.game.archetypes.species.PlantSpecies
import arx.axistential.game.data.entity.FoodData
import arx.axistential.game.data.entity.animal.EatingData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.logic.ai.goals.MoveGait
import arx.axistential.game.logic.ai.goals.MoveToRangeOfEffectGoal
import arx.axistential.modules.hunger.ActivityLevel
import arx.axistential.modules.hunger.MetabolicData
import arx.core.datastructures.OneOrMore
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.VoxelSideCoord

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/30/15
 * Time: 5:43 PM
 */

class GrazeGroundGoal(atLocations : OneOrMore[VoxelSideCoord]) extends PhysicalGoal {
	override def activityLevel: ActivityLevel = ActivityLevel.Light

	override def createPrerequisiteGoals(agent: TAIAgent): PrerequisiteGoalsResult = {
		MoveToRangeOfEffectGoal(agent,atLocations.head.adjacent,MoveGait.Saunter) :: Nil
	}

	override def progressRequired(agent: TAIAgent): Float = 4.0f

	override def split(agent: TAIAgent): SplitResult = atLocations.size match {
		case 1 => this
		case more => new GrazeGroundGoal(atLocations.head) -> new GrazeGroundGoal(atLocations.tail)
	}

	override def fitness(agent: TAIAgent): Int = AI.Fitness.Normal

	override def act(agent: TAIAgent, dt: UnitOfTime): AIResult = {
		val world = agent.world

		progress += dt.inSeconds
		if (progress >= progressRequired(agent)) {
			val loc = atLocations.head
			val TD = world.aux[TerrainData]
			val ED = agent.aux[EatingData]
			val covering = TD.coveringAt(loc)
			val arch = covering.archetype
			val state = covering.state
			arch match {
				case ps : PlantSpecies => {
					TD.coveringGrid.modificationBlock(loc.voxel) {
						if (state.isBitSet(Covering.CroppedShort)) {
							TD.setCoveringAt(loc,Covering.Sentinel)
						} else {
							val newCovering = Covering(arch,state | Covering.CroppedShort)
							TD.setCoveringAt(loc,newCovering)
						}
					}

					val foodProducts = ps.products.filter(p => ED.canEatEntity(p.archetype.exampleInstance))
					if (foodProducts.isEmpty) {
						Noto.warn(s"No food products in ground grazing... odd, species was $ps")
						Success
					} else {
						val newCalories = foodProducts.map(p =>
							p.archetype.exampleInstance.aux[FoodData].calories * p.number.resolve().toInt
						).sum

						agent.aux[MetabolicData].caloriesAvailable += newCalories
						Noto.info(s"grazing adding new cals: $newCalories, new total: ${agent.aux[MetabolicData].caloriesAvailable}")
					}

					Success
				}
				case _ => Fail(UnexpectedState("Voxel side targeted for grazing did not have a plant"))
			}
		} else {
			Continue
		}
	}
}
