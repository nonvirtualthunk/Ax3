package arx.eldr.game.archetypes

import arx.core.datastructures.MultiMap
import arx.core.datastructures.OneOrMore
import arx.core.introspection.ReflectionAssistant
import arx.core.introspection.TEagerSingleton
import arx.core.traits.ArxEnum
import arx.core.traits.TSentinel
import arx.core.traits.TSentinelable
import arx.core.vec.coordinates.VoxelCoord
import arx.eldr.game.requirements.AnyOneRequirement
import arx.eldr.game.requirements.Requirement
import arx.engine.entity.GameArchetype
import arx.engine.entity.GameEntity
import arx.engine.entity.TGameEntity
import arx.Prelude._
import arx.eldr.game.archetypes.Reaction.InputMap
import arx.eldr.game.archetypes.Reaction.OutputMap

/**
 * TODO: Add javadoc
 */

trait Reaction extends TSentinelable with TEagerSingleton {
	import Reaction.InputMap
	import Reaction.OutputMap

	def name : String

	var inputs : Map[InputKind,Requirement] = Map()
	var outputs : Map[OutputKind, ReactionOutput] = Map()
	var tools : Map[InputKind,Requirement] = Map()
	var location : List[Requirement] = Nil
	var actors : List[Requirement] = List(AnyOneRequirement)
	var associatedSkills : List[Skill] = Nil
	var baseTimeCost = 1.second
	def react(inputs: InputMap, tools: InputMap, location: VoxelCoord, actors: List[TGameEntity]): OutputMap

	// Reactions that this reaction is a variant or sub of. I.e. Chop Oak Tree would be a variant of the more
	// general Chop Tree
	var variantOf = none[Reaction]
	// Freeform indicator of what kind of reaction this is, intended for use by UI
	var categories = Set[String]()

	/** If true, indicates that this reaction applies to a singular, specific instance and should not be
	  * codified in the reaction dictionary */
	def isSpecificReaction = false

	/** If true, indicates that this is a base for other reactions, and should not be used directly itself */
	def baseReaction = false

	def createVariant(variantName : String, specific : Boolean) : Reaction = {
		new VariantReaction(this, variantName, specific)
	}

	if (!isSentinel && !isSpecificReaction) {
		Reaction.addReaction(this)
	}
}

case class ReactionOutput(archetype : GameArchetype, amount : Int)

class VariantReaction(varOf : Reaction, nomen : String, specific : Boolean) extends Reaction {
	variantOf = Some(varOf)
	inputs = varOf.inputs
	outputs = varOf.outputs
	tools = varOf.tools
	location = varOf.location
	actors = varOf.actors
	associatedSkills = varOf.associatedSkills
	baseTimeCost = varOf.baseTimeCost
	categories = varOf.categories

	var name = nomen
	override def react(inputs: InputMap, tools: InputMap, location: VoxelCoord, actors: List[TGameEntity]): OutputMap = {
		varOf.react(inputs,tools,location,actors)
	}

	/** If true, indicates that this reaction applies to a singular, specific instance and should not be
	  * codified in the reaction dictionary */
	override def isSpecificReaction: Boolean = specific
}

trait BaseReaction extends Reaction {
	override def baseReaction = true

	var variantRequirements : Map[InputKind,Requirement] = Map()
	def variantFor(variantInputs : Map[InputKind,TGameEntity]) : Reaction
}


object Reaction {
	type InputMap = MultiMap[InputKind,TGameEntity]
	type OutputMap = MultiMap[OutputKind,TGameEntity]

	val Sentinel = new Reaction with TSentinel {
		override def name: String = "sentinel reaction"
		override def react(inputs: InputMap, tools: InputMap, location: VoxelCoord, actors: List[TGameEntity]): OutputMap = new MultiMap
	}

	var allReactions = List[Reaction]()
	var reactionsByName = Map[String,Reaction]()

	def addReaction(r : Reaction): Unit ={
		allReactions ::= r
		reactionsByName += r.name -> r
	}
}

abstract class InputKind {}
object InputKind {
	case class Target(name : String) extends InputKind
	case class Normal(name : String) extends InputKind
	case class Tool(name : String) extends InputKind
}

abstract class OutputKind {}
object OutputKind {
	case class Product(name : String) extends OutputKind
	case class Material(name : String) extends OutputKind
	case class Byproduct(name : String) extends OutputKind
}