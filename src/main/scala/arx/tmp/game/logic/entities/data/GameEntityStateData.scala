package arx.tmp.game.logic.entities.data

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/17/13
 * Time: 10:13 AM
 * To change this template use File | Settings | File Templates.
 */

import arx.core.representation.InformationLevel

@SerialVersionUID(1L)
class GameEntityStateData extends TGameEntityAuxData {
	var _state = Set[String]()
	def state = _state
	def state_= ( s : Set[String] ) { _state = s }

	override def informationPairs(level: InformationLevel.InformationLevel): Map[String, Any] =
		state.zipWithIndex.map( tup => ("state" + tup._2) -> tup._1 ).toMap
}
object GameEntityStateData {
	val Sentinel : GameEntityStateData = new GameEntityStateData {
		override def state = Set()
		protected def readResolve : Object = GameEntityStateData.Sentinel
	}
}
