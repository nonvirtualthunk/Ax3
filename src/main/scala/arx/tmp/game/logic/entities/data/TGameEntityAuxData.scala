package arx.tmp.game.logic.entities.data

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/4/13
 * Time: 3:58 PM
 * Created by nonvirtualthunk
 */

import arx.application.Noto
import arx.core.representation.InformationLevel
import arx.tmp.game.logic.entities.core.TGameBase
import arx.tmp.game.logic.entities.core.THasAuxData

@SerialVersionUID(1L)
trait TGameEntityAuxData extends TAuxData with Serializable {

	/**
	 * Method for retrieving key-value pairs representing the data contained in this object, intended for
	 * ui use.
	 *
	 * @param level how fine the detail should be, can be used to determine what is returned (i.e. metadata is
	 *              only returned on Fine or higher levels)
	 * @return key-value pairs
	 */
	def informationPairs(level : InformationLevel.InformationLevel) : Map[String,Any] = Map()

	def onAssignedToEntity (entity : TGameBase) {}

	override def onAssignedToObject(entity: THasAuxData[_]): Unit = entity match {
		case geb : TGameBase => onAssignedToEntity(geb)
		case o => Noto.warn(s"Game entity aux data assigned to non-entity $o")
	}
}


