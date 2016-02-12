package arx.axistential.game.data.world

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/7/13
 * Time: 10:23 AM
 */

import arx.application.Noto
import arx.axistential.game.data.world.Biome.Moisture
import arx.axistential.game.data.world.Biome.Seasonality
import arx.axistential.game.data.world.Biome.Temperature
import arx.core.representation.ConfigValue
import arx.core.traits.ArxEnum
import arx.core.traits.TArxEnum
import arx.engine.data.TWorldAuxData
import arx.tmp.game.logic.descriptors.TDescriptor

class BiomeData extends TWorldAuxData {
	var mainBiome = new Biome
}

class Biome {
	var moisture = Biome.Moisture.Semihumid
	var temperature = Biome.Temperature.Temperate
	var seasonality = Biome.Seasonality.Moderate

	var seasonalRainfall = Map( Season.Summer -> 0.5f , Season.Autumn -> 0.2f , Season.Winter -> 0.1f , Season.Spring -> 0.2f )
}

class BiomeDescriptor(moistureRange : (Moisture,Moisture) , temperatureRange : (Temperature,Temperature) , seasonalityRange : (Seasonality,Seasonality) ) extends TDescriptor {
	def doesMatch(ref: Any): Boolean = {
		ref match {
			case biome : Biome => {
				val mm = biome.moisture.percent >= moistureRange._1.percent && biome.moisture.percent <= moistureRange._2.percent
				val tm = biome.temperature.percent >= temperatureRange._1.percent && biome.temperature.percent <= temperatureRange._2.percent
				val sm = biome.seasonality.percent >= seasonalityRange._1.percent && biome.seasonality.percent <= seasonalityRange._2.percent

				mm && tm && sm
			}
			case _ => false
		}
	}

	def exampleMatch: Serializable = new Biome()
}

object AnyBiome extends TDescriptor {
	def doesMatch(ref: Any): Boolean = {
		ref.isInstanceOf[Biome]
	}
	def exampleMatch: Serializable = new Biome
}

object BiomeDescriptor {
	def fromSML ( sml : ConfigValue ) = {
		var moistureRange = Biome.Moisture.Arid -> Biome.Moisture.Inundated
		var temperatureRange = Biome.Temperature.Arctic -> Biome.Temperature.Tropical
		var seasonalityRange = Biome.Seasonality.Equatorial -> Biome.Seasonality.High

		if ( sml.moistureRange.nonEmpty ) {
			moistureRange = TArxEnum.withKey(sml.moistureRange.arr(0).str.toLowerCase, Biome.Moisture.Arid) ->
										TArxEnum.withKey(sml.moistureRange.arr(1).str.toLowerCase, Biome.Moisture.Inundated)
		}

		if ( sml.temperatureRange.nonEmpty ) {
			temperatureRange = TArxEnum.withKey(sml.temperatureRange.arr(0).str.toLowerCase, Biome.Temperature.Arctic) ->
											TArxEnum.withKey(sml.temperatureRange.arr(1).str.toLowerCase, Biome.Temperature.Tropical)
		}

		if ( sml.seasonalityRange.nonEmpty ) {
			seasonalityRange = TArxEnum.withKey(sml.moistureRange.arr(0).str.toLowerCase, Biome.Seasonality.Equatorial) ->
											TArxEnum.withKey(sml.moistureRange.arr(1).str.toLowerCase, Biome.Seasonality.Extreme)
		}

		new BiomeDescriptor(moistureRange,temperatureRange,seasonalityRange)
	}
}

object Biome {
	class Moisture ( name : String , val percent : Float ) extends ArxEnum(name)

	object Moisture {
		val Arid = new Moisture("Arid",0.0f)
		val Semiarid = new Moisture("Semiarid",0.25f)
		val Semihumid = new Moisture("Semihumid",0.5f)
		val Humid = new Moisture("Humid",0.75f)
		val Inundated = new Moisture("Inundated",1.0f)
	}

	class Temperature ( name : String , val percent : Float ) extends ArxEnum(name)

	object Temperature {
		val Arctic = new Temperature("Arctic",0.0f)
		val Boreal = new Temperature("Boreal",0.25f)
		val Temperate = new Temperature("Temperate",0.5f)
		val Subtropical = new Temperature("Subtropical",0.75f)
		val Tropical = new Temperature("Tropical",1.0f)
		val Hellish = new Temperature("Hellish",1.25f)
	}

	class Seasonality ( name : String , val percent : Float ) extends ArxEnum(name)

	object Seasonality {
		val Equatorial = new Seasonality("Equatorial",0.0f)
		val Mild = new Seasonality("Mild",0.25f)
		val Moderate = new Seasonality("Moderate",0.5f)
		val High = new Seasonality("High",0.75f)
		val Extreme = new Seasonality("Extreme",1.0f)
	}
}

class Season ( name : String ) extends ArxEnum(name) {
	var followedBy : Season = this
	def next = followedBy
	def withFollowedBy (other : Season) = { followedBy = other ; this }
}
object Season {
	def fromString ( str : String ) = str.toLowerCase match {
		case "summer" => Summer
		case "autumn" => Autumn
		case "fall" => Autumn
		case "winter" => Winter
		case "spring" => Spring
		case other => Noto.warn(s"Unknown season name $other"); Summer
	}


	val Winter = new Season("Winter")
	val Autumn = new Season("Autumn").withFollowedBy(Winter)
	val Summer = new Season("Summer").withFollowedBy(Autumn)
	val Spring = new Season("Spring").withFollowedBy(Summer)
	Winter.withFollowedBy(Spring)


	val AllYear = Set(Summer,Autumn,Winter,Spring)
}