package arx.axistential.game.archetypes.species

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/7/13
 * Time: 2:48 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.archetypes.traits.TPhysicalEntityArchetype
import arx.axistential.game.data.entity.AgeCategory
import arx.axistential.game.data.entity.PlantKind
import arx.axistential.game.data.world.AnyBiome
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.physics.CollisionShape
import arx.axistential.graphics.graphicsinfo.ConfiguredGraphicsInfoProvider
import arx.core.Moddable
import arx.core.representation.ConfigValue
import arx.core.representation.TConfigurable
import arx.core.units._
import arx.tmp.game.logic.descriptors.AnyEntityDescriptor
import arx.tmp.game.logic.descriptors.TDescriptor
import arx.tmp.game.logic.entities.data.TArchetypeCollector




abstract class PlantSpecies extends Species {
	var plantKind : PlantKind = PlantKind.Vegetable
	def speciesKind = plantKind
	var biomeDescriptor : TDescriptor = AnyBiome

	import PlantSpecies._
	var lightUsage = PlantSpecies.LightUsage(LightUsage.PartialShade,LightUsage.MediumTolerance,0.5f)
	var waterUsage = PlantSpecies.WaterUsage(WaterUsage.Moist,WaterUsage.MediumTolerance)
	var nutrientUsage = PlantSpecies.NutrientUsage(NutrientUsage.Moderate)

	var rootRadius = 0.5.meters
	var leafRadius = 0.5.meters

	var associatedWith : List[PlantAssociation] = Nil

	var spreadChance = 1.0f
	var spreadFalloff = 0.5f
	var spreadNumber = 1

	/** Determines which parts of this plant should be harvested by a species, unless otherwise noted. Used to exclude
	  * twigs and leaves from normal harvesting by humans, for example. Generally animals will have their own harvesting
	  * rules based purely on what they can eat, so this applies almost exclusively to sentients */
	var defaultHarvestDescriptorBySpecies = Map[AnimalSpecies,TDescriptor]().withDefaultValue(AnyEntityDescriptor)
}

class PlantAssociation extends TConfigurable {
	var otherSpecies : Moddable[PlantSpecies] = PlantSpecies.Sentinel
	var chance : Float = 0.5f
	var distance : UnitOfDistance = 1.meter

	def setFromSML(sml: ConfigValue, overwrite: Boolean): Unit = {
		otherSpecies = () => PlantSpecies.archetypeWithName(sml.species.str)
		chance = sml.chance.floatOrElse(chance)
		distance = sml.distance.asDistance
	}
}









object PlantGraphicsInfoProvider extends ConfiguredGraphicsInfoProvider[PlantSpecies](PlantSpecies.allSML _) {

}


object PlantSpecies extends TArchetypeCollector[PlantSpecies] {
	val Sentinel : PlantSpecies = new PlantSpecies {
		protected def createPhysicalInstance: TPhysicalEntityArchetype.PhysicalArchetypedEntity = new TPhysicalEntity {}
		protected def readResolve : Object = PlantSpecies.Sentinel
		override def collisionShapeForAgeCategory(ageCategory: AgeCategory): CollisionShape = CollisionShape.Sentinel
	}

	protected def baseSMLLocation: String = "axis/entities/plants/Plants.sml"
	protected def typeKey: String = "plants"
	protected def clazz: Class[PlantSpecies] = classOf[PlantSpecies]
	protected def fromSML[U >: PlantSpecies](name : String,sml: ConfigValue): U = new ConfiguredPlantSpecies(name, sml)


	class LightUsage ( var lightPreference : Int , var tolerance : Int, var shadePcnt : Float )
	class WaterUsage ( var waterPreference : UnitOfDistance , var tolerance : UnitOfDistance ) {
		//1cm x 1m x 1m = 10kg
		def perYear = waterPreference
	}
	class NutrientUsage ( var perYear : Float )

	object LightUsage {
		def apply ( lightPreference : Int , tolerance : Int, shadePcnt : Float ) = new LightUsage(lightPreference,tolerance,shadePcnt)
		def fromSML ( sml : ConfigValue ) = {
			val preference = sml.lightPreference.unwrapped match {
				case f : Float => f.toInt
				case str : String => fromString(str)
				case _ => Noto.warn("No sunlight preference provided for plant");PartialSunlight
			}
			val tolerance = sml.lightTolerance.unwrapped match {
				case f : Float => f.toInt
				case str : String => this.tolerance(str)
				case _ => Noto.warn("No maximum sunlight provided for plant");PartialSunlight
			}
			val shadePcnt = sml.shadingPercent.floatOrElse(0.5f)
			
			this(preference,tolerance,shadePcnt)
		}

		val FullSunlight  = 32
		val PartialSunlight  = 26
		val PartialShade  = 20
		val Shade  = 14
		val FullShade  = 7
		val Darkness = 0

		val NoTolerance = 0
		val LowTolerance = 6
		val MediumTolerance = 12
		val HighTolerance = 18
		
		def fromString ( str : String ) = str.toLowerCase.stripWhitespace match {
			case "fullsunlight" => FullSunlight
			case "fullsun" => FullSunlight
			case "partialsunlight" => PartialSunlight
			case "partialsun" => PartialSunlight
			case "partialshade" => PartialShade
			case "shade" => Shade
			case "totalshade" => FullShade
			case "darkness" => Darkness
			case _ => Noto.warn(s"Unknown light usage string : $str"); 0
		}
		def tolerance ( str : String ) = str.toLowerCase.stripWhitespace match {
			case "none" => NoTolerance
			case "low" => LowTolerance
			case "medium" => MediumTolerance
			case "high" => HighTolerance
			case _ => Noto.warn(s"Unknown light tolerance string : $str"); 0
		}
	}
	
	object NutrientUsage {
		def apply ( perYear : Float ) = new NutrientUsage(perYear)
		def fromSML ( sml : ConfigValue ) = {
			val perYear = fromString(sml.nutrientUsage.strOrElse("moderate"))
			this(perYear)
		}

		val None = 0.0f
		val VeryLow = 2.5f
		val Low = 10.0f
		val MidLow = 15.0f
		val Moderate = 20.0f
		val MidHigh = 25.0f
		val High = 30.0f
		val VeryHigh = 40.0f
		
		def fromString ( str : String ) = str.toLowerCase match {
			case "none" => None
			case "very low" => VeryLow
			case "low" => Low
			case "mid-low" => MidLow
			case "moderate" => Moderate
			case "mid-high" => MidHigh
			case "high" => High
			case "very high" => VeryHigh
			case _ => Noto.warn(s"Unknown nutrient usage string : $str"); 0.0f
		}
	}
	
	object WaterUsage {
		def apply ( preference : UnitOfDistance, tolerance : UnitOfDistance ) = new WaterUsage(preference,tolerance)
		def fromSML ( sml : ConfigValue ) = {
			val preference = fromString(sml.moisturePreference.strOrElse("moist"))
			val tolerance = this.tolerance(sml.moistureTolerance.strOrElse("medium"))

			this(preference,tolerance)
		}
		
		val Parched = 0.0.cm
		val VeryDry = 20.cm
		val Dry = 40.cm
		val Moist = 60.cm
		val Wet = 80.cm
		val VeryWet = 100.cm
		val Inundated = 200.cm

		val NoTolerance = 0.cm
		val LowTolerance = 20.cm
		val MediumTolerance = 40.cm
		val HighTolerance = 60.cm

		def fromString ( str : String ) = str.toLowerCase match {
			case "parched" => Parched
			case "very dry" => VeryDry
			case "dry" => Dry
			case "moist" => Moist
			case "wet" => Wet
			case "very wet" => VeryWet
			case "inundated" => Inundated
			case _ => Noto.warn(s"Unknown water usage string : $str"); 0.cm
		}

		def tolerance ( str : String ) = str.toLowerCase.stripWhitespace match {
			case "none" => NoTolerance
			case "low" => LowTolerance
			case "medium" => MediumTolerance
			case "high" => HighTolerance
			case _ => Noto.warn(s"Unknown water tolerance string : $str"); MediumTolerance
		}
	}
	
}
