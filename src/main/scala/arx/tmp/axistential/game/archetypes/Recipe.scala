package arx.axistential.game.archetypes

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/15/13
 * Time: 8:21 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.archetypes.item.ItemArchetype
import arx.axistential.game.archetypes.sml.ConfiguredRecipe
import arx.core.Moddable
import arx.core.datastructures.MultiMap
import arx.core.function.SimpleExpression
import arx.tmp.game.logic.descriptors.TDescriptor
import arx.tmp.game.logic.entities.core.ConfigurableEntityAssistant
import arx.tmp.game.logic.entities.core.GameArchetype
import arx.tmp.game.logic.entities.core.GameEntity
import arx.requirements.TRequirement

import scala.collection.mutable

abstract class Recipe extends GameArchetype {
	//Fundamentally, a recipe has inputs, outputs, and contexts
	//Corresponding to, what goes in and is used up
	//what comes out as a result (or byproduct)
	//and the situations/locations in which it can be applied (and by whom)

	def inputRequirements : Map[String,Recipe.Input]
	def outputs : Map[String,Recipe.Output]

	def locationDescription : TDescriptor
	def crafterDescription : TDescriptor
	
	final def craft ( inputs : Map[String,List[GameEntity]] , location : GameEntity , crafter : GameEntity ) = {
		if ( (this.locationDescription.notSentinel && ! this.locationDescription.matches(location)) ) {
			Noto.warn(s"Invalid location when crafting recipe : $location $crafter")
		} else if ( (this.crafterDescription.notSentinel && ! this.crafterDescription.matches(crafter) ) ) {
			Noto.warn(s"Invalid crafter when crafting recipe : $location $crafter")
		} else {
			val inputReqs = inputRequirements
			for ( (name,req) <- inputReqs ) {
				val correspondingInputs = inputs(name)
				if ( correspondingInputs.fsum( ent => req.requirement.amountSatisfiedBy(ent) ) < req.requirement.amount ) {
					Noto.warn(s"Attempting to craft with invalid inputs")
				}
			}

			this.doCraft(inputs,location,crafter).toList
		}
	}
	
	protected def doCraft ( inputs : Map[String,List[GameEntity]] , location : GameEntity , crafter : GameEntity ) : Traversable[GameEntity] 
}


object Recipe {
	case class Output ( archetype : GameArchetype, chance : Float = 1.0f, number : Moddable[Float] = new SimpleExpression(1) )
	case class Input ( requirement : TRequirement , consumed : Boolean = true )


	protected var registeredRecipes = List[Recipe]()
	lazy val allSML = ConfigurableEntityAssistant.loadAllConfigsByPackage("axis/entities/items/Recipes.sml","recipes")
	protected lazy val _allRecipes = loadItemRecipes() ::: loadSMLArchetypes() ::: Nil
	lazy val recipesByOutput = recipesGroupedByOutput()
	def allRecipes = registeredRecipes ::: _allRecipes
	lazy val recipesByName = {
		val res = new mutable.HashMap[String,Recipe]()
		for (r <- allRecipes) { res.put(r.name,r) }
		res
	}
	def withName (name : String) = recipesByName(name)

	def registerRecipe ( recipe : Recipe ) : Recipe = {
		if (recipesByName.contains(recipe.name)) {
			Noto.warn(s"Duplicate recipe names : ${recipe.name}")
		}
		registeredRecipes ::= recipe
		recipesByName.put(recipe.name,recipe)
		recipe.outputs.values.foreach( output => recipesByOutput.add(output.archetype,recipe) )
		recipe
	}

	protected def loadItemRecipes () = {
		var ret : List[Recipe] = Nil
		for ( arch <- ItemArchetype.allArchetypes ) {
			ItemArchetype.allSML.get( arch.name.toLowerCase ) match {
				case Some(sml) => {
					var n = 1
					for ( value <- extractSingularOrPlural(sml,"recipe","recipes") ) {
						ret ::= new ConfiguredRecipe(s"Craft ${arch.name} [$n]}",value,Some(arch))
						n += 1
					}
				}
				case None => Noto.warn(s"No SML for arch $arch")
			}
		}
		ret
	}

	protected def recipesGroupedByOutput() = {
		val ret = new MultiMap[GameArchetype,Recipe]
		_allRecipes.foreach( recipe => recipe.outputs.values.foreach( output => ret.add(output.archetype,recipe) ) )
		ret
	}


	def loadSMLArchetypes () = {
		val smlArchs = allSML.map { case (name,sml) => new ConfiguredRecipe(name.toString,sml,None) }

		smlArchs.toList
	}
}