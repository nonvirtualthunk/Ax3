package arx.tmp.game.logic.entities.data

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/4/13
 * Time: 4:02 PM
 * Created by nonvirtualthunk
 */

import arx.core.representation.InformationLevel

@SerialVersionUID(1L)
class GameEntityMetadata extends TGameEntityAuxData {
	var _metadata = Map[String,Any]()
	def metadata = _metadata
	def metadata_= ( m : Map[String,Any] ) { _metadata = m }

	override def informationPairs (level : InformationLevel.InformationLevel) = level match {
		case InformationLevel.Info => Map()
		case _ => metadata
	}
}

object SentinelGameEntityMetadata extends GameEntityMetadata {
	override def metadata = Map()
	override def metadata_= ( m : Map[String,Any] ) { }
}