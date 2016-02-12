package arx.axistential.modules.hunger

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 9/21/14
 * Time: 2:37 PM
 */

import arx.Prelude._
import arx.application.SimpleLoggingLevelProvider
import arx.core.representation.InformationLevel
import arx.core.representation.InformationLevel.InformationLevel
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.data.TGameEntityAuxData

class MetabolicData extends TGameEntityAuxData {
	/** Calories currently available for usage */
	var caloriesAvailable = 1000.0f
	/** Base rate at which calories are consumed just maintaining homeostasis, per second */
	var baseMetabolicRate = Moddable(1.5f)
	/** The rate at which calories are consumed by activity, per second*/
	var activityMetabolicRate = Moddable(0.5f)
	/** Maximum calories that this entity can hold */
	var maxCaloriesStored = baseMetabolicRate * 1.25.cycle.inSeconds // default to 1.25 days of storage
	
	def effectiveMetabolicRate (activityLevel : ActivityLevel) = {
		baseMetabolicRate + activityMetabolicRate * activityLevel.metabolicRateMultiplier
	}

	override def informationPairs(level: InformationLevel) = Map()
}

object MetabolicData {
	implicit def entityToMetabolicData (ge : GameEntity) : MetabolicData = ge.aux[MetabolicData]
}
object MetabolicLogging extends SimpleLoggingLevelProvider

case class ActivityLevel (metabolicRateMultiplier : Float){
	
}
object ActivityLevel {
	val None = ActivityLevel(0.0f)
	val Minimal = ActivityLevel(0.25f)
	val VeryLight = ActivityLevel(0.5f)
	val Light = ActivityLevel(1.0f)
	val Moderate = ActivityLevel(1.5f)
	val Heavy = ActivityLevel(2.5f)
	val Extreme = ActivityLevel(4.0f)
}