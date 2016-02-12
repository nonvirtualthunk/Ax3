package arx.axistential.game.data.entity.animal

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/11/15
 * Time: 9:27 AM
 */

import arx.core.representation.ConfigValue
import arx.core.representation.InformationLevel.InformationLevel
import arx.tmp.game.logic.descriptors.ConfiguredDescriptor
import arx.tmp.game.logic.descriptors.TDescriptor
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.data.TConfigurableGameEntityAuxData
import arx.tmp.game.logic.entities.data.TInheritableAuxData

@SerialVersionUID(1L)
class EatingData extends TConfigurableGameEntityAuxData with TInheritableAuxData {
	var dietType = DietType.defaultValue
	var canEat = Set[TDescriptor]()

	def canEatEntity (entity : GameEntity) = canEat.exists(d => d.matches(entity))

	override def copyToInstance(entity: GameEntity): Unit = {
		val ED = entity.aux[EatingData]
		ED.canEat = this.canEat
		ED.dietType = this.dietType
	}


	override def createFromSML(sml: ConfigValue): Option[EatingData] = {
		sml.canEat match {
			case ConfigValue.Sentinel => None
			case _ => {
				val ret = new EatingData
				for (eatSML <- sml.canEat.arr) {
					ret.canEat += ConfiguredDescriptor(eatSML)
				}
				ret.dietType = DietType(sml.dietType.str)

				Some(ret)
			}
		}
	}

	override def informationPairs(level: InformationLevel): Map[String, Any] = Map()
}