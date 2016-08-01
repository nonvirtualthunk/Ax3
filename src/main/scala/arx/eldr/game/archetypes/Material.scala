package arx.eldr.game.archetypes

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/6/13
  * Time: 12:13 PM
  * To change this template use File | Settings | File Templates.
  */

import arx.Prelude._
import arx.core.representation.ConfigAssistant
import arx.core.representation.ConfigValue
import arx.core.traits._
import arx.core.units.UnitOfDensity
import arx.core.units.UnitOfMeasure
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec3i
import arx.eldr.game.entity.data.CubeShape
import arx.eldr.game.entity.data.PhysicalData
import arx.engine.data.TGameEntityAuxData
import arx.engine.entity._

import scala.collection.mutable


// There are generally two things we are concerned with when we are talking about
// archetypes. The first is the creation of new instances and the second is finding
// information about the general class of thing. Say during world creation we want
// to pick a new plant to put in the world. We first need to identify a plant species
// that is able to survive in the given clime, then we need to actually create one
// with the appropriate characteristics as determined by that species and put it in
// the world.

// We have made some attempts to unify this by having aux-data that applies to both
// the archetype and instance, i.e. when an instance is created the aux data is copied
// from the arch to the inst. That's not entirely perfect, since aux data can generally
// contain variables and randomized parts that we would want to solidify on instantiating
// but that's probably somewhat manageable, or handleable by special case. We also
// have an interesting issue in that we want to make pretty much everything modifiable.
// Water consumption is presumably a feature of the species as a whole, but we would want
// that to be alterable both at creation time (genetics!) or midway through its life
// span (magic!). So many of the things that seem to naturally fall into the archetype
// actually need to be a part of the individual entity (from a theoretical perspective
// at least, they could be copy on write or something in practice). Things that are
// part of the archetype pretty much for sure are just the soft info: name, description.
// So the separation once an instance is created is quite significant. Since entities
// are now entirely defined by their aux-data, not by subclassing, creation of new
// instances becomes comparatively trivial, and we could probably separate that out
// from the archetypes entirely. We could, instead, define Creators, with the default
// creator simply taking any appropriately marked aux-data from the archetype and
// propagating it to the child. Could have the creator interface also take a list
// of "parent" game entities, to be dealt with on a case by case basis. So, generally
// speaking archetypes are just slightly modified game entities, that are referenced
// purely by identifier (so serialization happens by name rather than contents). Most
// entities will be created from an archetype of some kind, but probably not all...
// so how do we store it? An aux data seems sort of like overkill, well, lets just
// make a function on the game entity interface, leave it to implementors

class Material(val baseName: String, val flags: Set[MaterialFlag] = Set())
	extends GameArchetype(Material.nameFrom(baseName, flags), Material) {
	displayName = baseName
	var density: UnitOfDensity = kg_m3
	var fluidPassable = false
	def conditionallyPassable = false
	protected var _opacity = 1.0f
	protected var _byteTransparency = 0.toByte
	def opacity = _opacity
	def opacity_=(o: Float) { _opacity = o; _byteTransparency = (-16 * o).toByte }
	def byteTransparency = _byteTransparency

	var solid = true
	var stateOfMatter = StateOfMatter.Solid
	var strength = 10.0f
	//scale [0.0f,100.0f]
	var insulation = 5.0f

	var horizontalSupportStrength = 5.kg
	var verticalSupportStrength = 5.kg

	var materialTypes = Set[MaterialType]()

	def copy(withFlags: Set[MaterialFlag]) = {
		val c = new Material(name, withFlags)
		c.density = density
		c.opacity = opacity
		c.solid = solid
		c.stateOfMatter = stateOfMatter
		c.strength = strength
		c.materialTypes = materialTypes
		c.displayName = displayName
		c.horizontalSupportStrength = horizontalSupportStrength
		c.verticalSupportStrength = verticalSupportStrength
		c.insulation = insulation
		c.fluidPassable = fluidPassable
		c
	}

	def withFlag(f: MaterialFlag): Material = {
		if (!flags(f)) {
			Material.materialVariantByFlags(this, flags + f)
		} else {this}
	}
	def withoutFlag(f: MaterialFlag): Material = {
		if (flags(f)) {
			Material.materialVariantByFlags(this, flags - f)
		} else {this}
	}
	def withoutFlags: Material = if (flags.isEmpty) {this} else {Material.materialVariantByFlags(this, Set())}

	def createMaterialBlock (amount : Int = 1) = {
		val ret = new MaterialBlock(this, amount)
		ret[PhysicalData].shape = CubeShape(Vec3i.One)
		ret[PhysicalData].staticObject = false
		ret
	}
}

class MaterialBlockData extends TGameEntityAuxData {
	var material : Material = Material.Sentinel
	var amount : Int = 1
}

class MaterialBlock(material : Material, amount : Int) extends MinimalGameEntity(TGameEntity.IdCounter.getAndIncrement) {
	this.aux[MaterialBlockData].material = material
	this.aux[MaterialBlockData].amount = amount
	override def name: String = this.aux[MaterialBlockData].material.displayName
}

object ConfiguredMaterial {
	def createFrom(name: String, conf: ConfigValue) = {
		val flags = if (conf.hasField("flags")) {
			conf.flags.arr.map(elem => MaterialFlag(elem.str)).toSet
		} else {
			Set()
		}

		val mat = new Material(name)

		mat.opacity = conf.opacity.floatOrElse(mat.opacity)
		mat.solid = conf.solid.boolOrElse(mat.solid)
		mat.stateOfMatter = StateOfMatter.fromString(conf.stateOfMatter.strOrElse("solid"))
		mat.density = conf.density.asDensity
		val horiRaw = UnitOfMeasure.parseUnitOfMass(conf.horizontalSupport.str)
		val vertRaw = UnitOfMeasure.parseUnitOfMass(conf.verticalSupport.str)
		mat.horizontalSupportStrength = horiRaw
		mat.verticalSupportStrength = vertRaw

		if (conf.hasField("types")) {
			conf.types.arr.foreach(elem => mat.materialTypes += MaterialType(elem.str))
		}
		mat.insulation = conf.insulation.floatOrElse(mat.insulation)
		mat
	}
}

object Material extends TArchetypeKind {
	val baseConfLocation: String = "eldr/entities/materials/Materials.conf"
	GameArchetype.loadAllArchetypes(baseConfLocation, "materials", ConfiguredMaterial.createFrom)


	val Sentinel: Material = new Material("Sentinel") with TSentinel {
		opacity = 0.0f
		solid = false
		protected def readResolve: Object = Material.Sentinel
	}

	val Engraved = MaterialFlag.Engraved
	val Smoothed = MaterialFlag.Smoothed
	val Tilled = MaterialFlag.Tilled
	val Constructed = MaterialFlag.Constructed

	val materialVariantsByFlagsMap = new mutable.HashMap[(String, Set[MaterialFlag]), Material]
	def materialVariantByFlags(material: Material, flags: Set[MaterialFlag]) = {

		if (flags.isEmpty) {
			Material.withName(material.baseName)
		} else {
			materialVariantsByFlagsMap.getOrElseUpdate(material.baseName -> flags, material.copy(flags))
		}
	}

	def materialWithName(name: String) = withName(name.toLowerCase)
	def withName(name: String) = GameArchetype.archetype(Material, name).asInstanceOf[Material]

	def nameFrom(baseName:String, flags : Set[MaterialFlag]) = {
		baseName + (if (flags.isEmpty) { "" } else { flags })
	}

	val Organic = MaterialType("Organic")
	val Ore = MaterialType("Ore")
	val Metallic = MaterialType("Metallic")
	val Ceramic = MaterialType("Ceramic")
	val Soil = MaterialType("Soil")
	val Stone = MaterialType("Stone")
	val Fabric = MaterialType("Fabric")
	val Vegetative = Set(MaterialType("Vegetative"), Organic)
	val Wood = MaterialType("Wood")
	val Crystalline = MaterialType("Crystalline")
}


class MaterialFlag protected(name: String) extends ArxEnum(name) {

}

object MaterialFlag {
	def apply(str: String) = {
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

class MaterialType protected(name: String) extends ArxEnum(name) {
}

object MaterialType extends ArxEnumObject[MaterialType] {
	override def apply(str: String): MaterialType = super.apply(str)
}

