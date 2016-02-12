package arx.axistential.game.archetypes

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/6/13
 * Time: 12:13 PM
 * To change this template use File | Settings | File Templates.
 */

import java.io.ObjectInputStream
import java.io.ObjectOutputStream

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.archetypes.traits.TPhysicalEntityArchetype
import arx.axistential.game.constants.StateOfMatter
import arx.axistential.game.entities.MaterialBlock
import arx.core.representation.ConfigValue
import arx.core.representation.TConfigurable
import arx.core.traits.ArxEnum
import arx.core.traits.ArxEnumObject
import arx.core.traits.TArxEnum
import arx.core.traits.TSentinel
import arx.core.units.UnitOfDensity
import arx.core.units.UnitOfMeasure
import arx.tmp.game.logic.entities.core.GameArchetype
import arx.tmp.game.logic.entities.data.TArchetypeCollector

import scala.collection.mutable

class Material extends GameArchetype with TPhysicalEntityArchetype {
	var baseName = "" //the name, minus any alterations for flags, etc
	var density : UnitOfDensity = kg_m3
	var fluidPassable = false
	def conditionallyPassable = false
	protected var _opacity = 1.0f
	protected var _byteTransparency = 0.toByte
	def opacity = _opacity
	def opacity_= ( o : Float ) { _opacity = o; _byteTransparency = (-16 * o).toByte }
	def byteTransparency = _byteTransparency

	var solid = true
	var stateOfMatter = StateOfMatter.Solid
	var strength = 10.0f //scale [0.0f,100.0f]
	var insulation = 5.0f

	var horizontalSupportStrength = 5.kg
	var verticalSupportStrength = 5.kg

	var materialTypes = Set[MaterialType]()
	var _flags : Set[MaterialFlag] = Set()
	def flags = _flags
	protected def flags_= ( f : Set[MaterialFlag] ) { _flags = f ; updateIdentifier() }
	override def name_= ( n : String ) { super.name_=(n); updateIdentifier() }

	protected var _identifier = name + flags
	override def identifier = _identifier
	protected var _hashCode = identifier.hashCode
	override def hashCode = _hashCode
	protected def updateIdentifier () { 
		_identifier = name + flags
		_hashCode = identifier.hashCode
	}

	def copy = {
		val c = new Material
		c.density = density
		c.opacity = opacity
		c.solid = solid
		c.stateOfMatter = stateOfMatter
		c.strength = strength
		c.materialTypes = materialTypes
		c.flags = flags
		c.name = name + " copy"
		c.baseName = baseName
		c.horizontalSupportStrength = horizontalSupportStrength
		c.verticalSupportStrength = verticalSupportStrength
		c.insulation = insulation
		c.fluidPassable = fluidPassable
		c
	}

	def withFlag ( f : MaterialFlag ) : Material = {
		if ( ! flags(f) ) {
			Material.materialVariantByFlags(this,flags + f)
		} else { this }
	}
	def withoutFlag ( f : MaterialFlag ) : Material = {
		if ( flags(f) ) {
			Material.materialVariantByFlags(this,flags - f)
		} else { this }
	}
	def withoutFlags : Material = if ( flags.isEmpty ) { this } else { Material.materialVariantByFlags(this,Set()) }

	protected def createPhysicalInstance = new MaterialBlock(this)
	@transient var _exampleBlock : MaterialBlock = _
	def exampleBlock = {if (_exampleBlock == null) { _exampleBlock = createPhysicalInstance } ; _exampleBlock }
}

class ConfiguredMaterial(initialSML : ConfigValue,nomen : String) extends Material with TConfigurable {
	name = nomen
	setFromSML(initialSML)

	def setFromSML(sml: ConfigValue, overwrite: Boolean): Unit = {
		name = sml.name.strOrElse(name)
		baseName = name
		opacity = sml.opacity.floatOrElse(opacity)
		solid = sml.solid.boolOrElse(solid)
		stateOfMatter = StateOfMatter.fromString( sml.stateOfMatter.strOrElse("solid") )
		density = sml.density.asDensity
		val horiRaw = UnitOfMeasure.parseUnitOfMass(sml.horizontalSupport.str)
		val vertRaw = UnitOfMeasure.parseUnitOfMass(sml.verticalSupport.str)
		horizontalSupportStrength = horiRaw
		verticalSupportStrength = vertRaw
		if ( sml.hasField("flags") ) {
			Noto.warn("Flags probably shouldn't be included as part of material SML")
			sml.flags.arr.foreach ( elem => flags += MaterialFlag(elem.str) )
		}
		if ( sml.hasField("types") ) {
			sml.types.arr.foreach ( elem => materialTypes += MaterialType(elem.str) )
		}
		insulation = sml.insulation.floatOrElse(insulation)
		updateIdentifier()
	}

	protected def readResolve : Object = Material.withName(this.name)
	private def writeObject (stream : ObjectOutputStream) { stream.writeUTF(name) }
	private def readObject (stream : ObjectInputStream) { name = stream.readUTF() }
}

object Material extends TArchetypeCollector[Material] {
	val Sentinel : Material = new Material with TSentinel {
		name = "Sentinel Material"
		opacity = 0.0f
		solid = false
		protected def readResolve : Object = Material.Sentinel
	}

	val Engraved = MaterialFlag.Engraved
	val Smoothed = MaterialFlag.Smoothed
	val Tilled = MaterialFlag.Tilled
	val Constructed = MaterialFlag.Constructed

	val materialVariantsByFlagsMap = new mutable.HashMap[(String,Set[MaterialFlag]),Material]
	def materialVariantByFlags( material : Material , flags : Set[MaterialFlag] ) = {

		if ( flags.isEmpty ) {
			Material.withName(material.baseName)
		} else {
			materialVariantsByFlagsMap.getOrElseUpdate(material.baseName -> flags, {
				val c = material.copy
				c.flags = flags
				c.name = flags.map(_.name).reduceLeft(_ + " " + _) + " " + material.baseName

				c
			})
		}
	}
	override protected def baseSMLLocation: String = "axis/entities/materials/Materials.sml"
	override protected def typeKey: String = "materials"
	override protected def fromSML[U >: Material](name : String,sml: ConfigValue): U = { new ConfiguredMaterial(sml,name) }
	override protected def clazz: Class[Material] = classOf[Material]

	def materialWithName ( name : String ) = withName( name.toLowerCase )
	
	val Organic = MaterialType("Organic")
	val Ore = MaterialType("Ore")
	val Metallic = MaterialType("Metallic")
	val Ceramic = MaterialType("Ceramic")
	val Soil = MaterialType("Soil")
	val Stone = MaterialType("Stone")
	val Fabric = MaterialType("Fabric")
	val Vegetative = Set(MaterialType("Vegetative"),Organic)
	val Wood = MaterialType("Wood")
	val Crystalline = MaterialType("Crystalline")
}



class MaterialFlag protected ( name : String ) extends ArxEnum(name) {

}

object MaterialFlag {
	def apply ( str : String ) = {
		TArxEnum.existing[MaterialFlag](str.toLowerCase) match {
			case Some(f) => f
			case None => new MaterialFlag(str)
		}
	}

	val Engraved = MaterialFlag("Engraved")
	val Smoothed = MaterialFlag("Smoothed")
	val Tilled = MaterialFlag("Tilled")
	val Constructed = MaterialFlag("Constructed")
}

class MaterialType protected ( name : String ) extends ArxEnum(name) {
}

object MaterialType extends ArxEnumObject[MaterialType] {
	override def apply(str: String): MaterialType = super.apply (str)
}

