package arx.axistential.game.data.world.networking

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/25/15
 * Time: 7:24 AM
 */

import arx.axistential.game.data.world.TerrainData
import arx.core.datastructures.ByteMapping
import arx.tmp.game.logic.world.data.NetworkedWorldDataUpdate
import arx.tmp.game.logic.world.util.TIdentifiable
import arx.tmp.game.networking._
import com.esotericsoftware.minlog.Log

class TerrainDataDeltaHandler(data:TerrainData) extends NetworkedWorldDataDeltaHandler[TerrainData](data) {
	addHandler(new ByteMappingDeltaHandler(data._materialMapping))
	addHandler(new ByteMappingDeltaHandler(data._coveringTypes))
	watchGrid(data._materialGrid)
	watchGrid(data._coveringGrid)
}

case class ByteMappingDelta[T](mappingId : Int, newMappings : Seq[(Byte,T)]) extends NetworkedWorldDataUpdate(mappingId)

class ByteMappingDeltaHandler[T <: TIdentifiable](mapping : ByteMapping[T]) extends TNetworkedDataDeltaHandler {
	var lastSize = 0
	override def extractLatestDeltas: Seq[NetworkedWorldDataUpdate] = {
		if (mapping.size > lastSize) {
			val newSize = mapping.size
			val changes = (lastSize until newSize).map(i => i.toByte -> mapping(i))
			lastSize = newSize
			Seq(ByteMappingDelta[T](fieldId,changes))
		} else {
			Nil
		}
	}

	override def applyLatestDelta(delta: NetworkedWorldDataUpdate): Boolean = {
		delta match {
			case ByteMappingDelta(msgId,newMappings) =>
				if (msgId == fieldId) {
					for ((index,t) <- newMappings) {
						mapping.set(index,t.asInstanceOf[T])
					}
					true
				} else {
					Log.error("serialization",s"Invalid delta given to byte mapping delta handler, had id $msgId, should have been $fieldId")
					false
				}
			case _ => false
		}
	}
}