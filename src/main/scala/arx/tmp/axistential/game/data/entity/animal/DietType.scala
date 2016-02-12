package arx.axistential.game.data.entity.animal

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 4/11/15
 * Time: 8:31 AM
 */

import arx.core.traits.ArxEnum
import arx.core.traits.ArxEnumObjectWithDefault

class DietType(nomen : String, val eatsLiveAnimals : Boolean) extends ArxEnum(nomen) {

}

object DietType extends ArxEnumObjectWithDefault[DietType] {
	val Omnivore = new DietType("omnivore",true)
	val Carnivore = new DietType("carnivore",true)
	val Scavenger = new DietType("scavenger",false)
	val Herbivore = new DietType("herbivore",false)
	val Unknown = new DietType("unknown",false)

	override def defaultValue: DietType = Unknown
}