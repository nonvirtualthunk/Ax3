package arx.axistential.game.archetypes.sml

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/21/13
 * Time: 10:39 AM
 * To change this template use File | Settings | File Templates.
 */

import java.io.ObjectInputStream
import java.io.ObjectOutputStream

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.archetypes.Recipe.Input
import arx.axistential.game.archetypes.Recipe.Output
import arx.axistential.game.archetypes.item.ConfiguredItemArchetype
import arx.axistential.game.archetypes.item.ItemArchetype
import arx.axistential.game.archetypes.Material
import arx.axistential.game.archetypes.Recipe
import arx.axistential.game.data.helpers.MaterialComposition
import arx.axistential.game.entities.MaterialBlock
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.requirements.ConfiguredRequirement
import arx.core.function.SimpleExpression
import arx.core.function.TArithmeticExpression
import arx.core.representation.ConfigValue
import arx.core.representation.TConfigurable
import arx.tmp.game.logic.descriptors.ConfiguredDescriptor
import arx.tmp.game.logic.descriptors.EntityWithArchetypeDescriptor
import arx.tmp.game.logic.descriptors.TDescriptor
import arx.tmp.game.logic.entities.core.GameArchetype
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.core.TInstantiableArchetype
import arx.requirements.NumberOfEntitiesRequirement

class ConfiguredRecipe(nomen: String, initialSML:ConfigValue, inputArchetypes : List[GameArchetype] , outputArchetypes : List[Output]) extends Recipe with TConfigurable {
	def this (nomen : String,initSML : ConfigValue, outputArchetype : Option[GameArchetype]) { this(nomen,initSML,Nil,outputArchetype.toList.map( t => Output(t,1.0f,new SimpleExpression(1.0f)) )) }
	this.name = nomen

	var sml = initialSML
	setFromSML(initialSML)

	def setFromSML(sml: ConfigValue, overwrite: Boolean): Unit = {
		this.sml = sml
	}

	var _inputRequirements = memoizeSingle( (s:ConfigValue) => {
		var ret : Map[String,Input] = Map()
		for ( input <- inputArchetypes ) {
			val key = if (inputArchetypes.size == 1) { "Main Input" } else { "Main Input " + (ret.size+1) }
			ret += key -> Input( new NumberOfEntitiesRequirement(new EntityWithArchetypeDescriptor(input),1) )
		}
		for ( input <- extractSingularOrPlural(sml,"input","inputs") ) {
			var consumed = true
			var inputName = s"Input ${ret.size + 1}"
			if ( input.isObj ) {
				consumed = input.consumed.boolOrElse(consumed)
				inputName = input.name.strOrElse(inputName)
			}
			val req = ConfiguredRequirement.fromSML(input)
			ret += inputName -> Input(req,consumed)
		}
		ret
	} )
	def inputRequirements: Map[String, Input] = _inputRequirements(sml)

	var _outputs = memoizeSingle( (s:ConfigValue) => {
		var ret : Map[String,Output] = Map()
		for ( output <- outputArchetypes ) {
			val key = if (outputArchetypes.size == 1) { "Main Output" } else { s"Main Output ${ret.size + 1}" }
			ret += key -> output
		}
		for ( output <- extractSingularOrPlural(sml,"output","outputs") ) {
			var defaultName = "output"
			val arch = if ( output.material.nonEmpty ) {
				defaultName = output.material.str
				Material.withName(output.material.str)
			} else if ( output.item.nonEmpty ) {
				defaultName = output.item.str
				ItemArchetype.archetypeWithName(output.item.str)
			} else {
				new ConfiguredItemArchetype(output)
			}

			val amount : TArithmeticExpression = output.amount.expressionOrElse(1)
			ret += output.name.strOrElse(defaultName) -> Output(arch, output.chance.floatOrElse(1.0f), amount)
		}
		ret
	} )
	def outputs: Map[String, Output] = _outputs(sml)

	var _locationDescription = memoizeSingle( (s:ConfigValue) => {
		if ( s.hasField("location") ) {
			ConfiguredDescriptor(s.location)
		} else {
			TDescriptor.Sentinel
		}
	})
	def locationDescription: TDescriptor = _locationDescription(sml)

	var _crafterDescription = memoizeSingle( (s:ConfigValue) => {
		if ( s.hasField("crafter") ) {
			ConfiguredDescriptor(s.crafter)
		} else {
			TDescriptor.Sentinel
		}
	})
	def crafterDescription: TDescriptor = _crafterDescription(sml)

	protected def doCraft(inputs: Map[String, List[GameEntity]], location: GameEntity, crafter: GameEntity): Traversable[GameEntity] = {
		outputs.values.map ( output => output.archetype match {
			case instanceable : TInstantiableArchetype => {
				val inst = instanceable.createInstance
				inst match {
					case tpe : TPhysicalEntity => {
						val allInputEntities = inputs.values.flatten.toList
						val allMaterials = allInputEntities.ofType[MaterialBlock].groupBy( _.material )
						if ( allMaterials.nonEmpty ) {
							tpe.materialComposition = List( MaterialComposition( allMaterials.toList.maxBy( _._2.size )._1 , 1.0f ) )
						}
					}
					case _ => //do nothing further
				}
				inst
			}
			case _ => Noto.warn("Attempted to craft a non instanceable archetype"); GameEntity.Sentinel
		} )
	}

	protected def readResolve : Object = Recipe.withName(this.name)
	private def writeObject (stream : ObjectOutputStream) { stream.writeUTF(name) }
	private def readObject (stream : ObjectInputStream) { name = stream.readUTF() }
}