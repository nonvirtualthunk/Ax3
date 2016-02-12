package arx.axistential.game.logic.ai.goals

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/24/13
 * Time: 11:27 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.ai._
import arx.axistential.game.archetypes.Recipe
import arx.axistential.game.archetypes.item.ItemArchetype
import arx.axistential.game.data.entity.InventoryData
import arx.axistential.game.entities.helpers.ConstructionSite
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.general.Location.EntityLocation
import arx.axistential.game.logic.general.Location.VoxelLocation
import arx.axistential.game.logic.general.EntityLogic
import arx.axistential.game.logic.general.Location
import arx.axistential.modules.hunger.ActivityLevel
import arx.core.units.UnitOfTime
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.core.TInstantiableArchetype

case class CraftRecipeGoal(recipe : Recipe, location : Location, crafter : Option[TPhysicalEntity]) extends PhysicalGoal {
	override def activityLevel: ActivityLevel = ActivityLevel.Moderate
	val outputs = recipe.outputs.values.map( output => output.archetype match {
		case instanceable : TInstantiableArchetype => instanceable.createInstance
		case _ => Noto.warn("No instanceable archetype in recipe output...baffling"); GameEntity.Sentinel
	} )

	val hasFreeStandingOutput = recipe.outputs.exists{ output => output._2.archetype match {
		case ia : ItemArchetype => ia.freeStanding
		case _ => false
	} }
	val constructionSite : TPhysicalEntity = location match {
		case vl : VoxelLocation => ConstructionSite(vl.centerPoint,outputs.find( o => o.archetype match { case ia : ItemArchetype if ia.freeStanding => true ; case _ => false } ).getOrElse(GameEntity.Sentinel))
		case el : EntityLocation => el.entity
	}
	if ( ! constructionSite.inWorld ) { location.world.addEntity( constructionSite ) }

	def createPrerequisiteGoals(agent: TAIAgent): PrerequisiteGoalsResult = {
		FetchRequirementsGoal(recipe.inputRequirements.values.map(_.requirement).toList,Some(constructionSite),false) :: Nil
	}

	def fitness(agent: TAIAgent): Int = crafter match {
		case Some(chosenCrafter) => if ( chosenCrafter == agent ) { AI.Fitness.Normal } else { AI.Fitness.Minimum }
		case _ => AI.Fitness.Normal
	}

	/** If this goal can be split into more than one part, create two new goals, the first representing
	  * the split off portion, the second representing the remainder
 *
	  * @param agent the agent who is to perform the split off section
	  */
	def split(agent: TAIAgent): SplitResult = this

	def act(agent: TAIAgent, dt : UnitOfTime): AIResult = {
		progress += dt.inSeconds * 60.0f
		constructionSite match {
			case cs : ConstructionSite => cs.percentComplete = progress / progressRequired(agent)
			case _ =>
		}

		if ( progress >= progressRequired(agent) ) {
			if ( hasFreeStandingOutput ) {

				for ( output <- outputs ) {
					output.archetype match {
						case ia : ItemArchetype if ia.freeStanding => {
							output match {
								case pe : TPhysicalEntity => pe.position = constructionSite match {
									case cs : ConstructionSite => cs.position
									case _ => location.centerPoint
								}
								case _ => Noto.warn("Recipe produced non physical entity, we didn't expect that")
							}
						}
						case oa => output match {
							case pe : TPhysicalEntity =>
								val held = agent.aux[InventoryData].holdEntityIfPossible(pe)
								posit(held,"Could not hold entity, that's bad")
							case _ => Noto.warn("Recipe produced non physical entity, we didn't expect that")
						}
					}

					constructionSite match {
						case cs : ConstructionSite => agent.world.removeEntity(cs)
						case _ =>
					}

					agent.world.addEntity(output)
				}
			} else {
				Noto.warn("No Freestanding output")
			}

			AIResult.Success
		} else { AIResult.Continue }
	}

	def progressRequired(agent: TAIAgent): Float = 300

	override def onFail (agent:TAIAgent, reason : AIReason) {
		constructionSite match {
			case cs : ConstructionSite => {
				EntityLogic.destroyEntity(cs)
			}
			case _ => //do nothing
		}
	}
}