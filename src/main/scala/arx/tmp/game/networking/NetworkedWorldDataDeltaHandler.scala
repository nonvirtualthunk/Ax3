package arx.tmp.game.networking

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/24/15
 * Time: 8:40 PM
 */

import arx.tmp.game.logic.datastructures.GenericTaleaGrid
import arx.tmp.game.logic.datastructures.TGridWithChangeQueue
import arx.tmp.game.logic.datastructures.TLoggingTalea
import arx.tmp.game.logic.world.data.NetworkedWorldDataUpdate
import arx.tmp.game.logic.world.data.TNetworkedWorldData

abstract class NetworkedWorldDataDeltaHandler[T <: TNetworkedWorldData](data : T)
	extends TNetworkedDataDeltaHandler
{
	var subHandlers = Vector[TNetworkedDataDeltaHandler]()


	final def extractLatestDeltas : Seq[NetworkedWorldDataUpdate] = {
		var ret = extractLatestDeltasImpl
		for (extract <- subHandlers) {
			for (mod <- extract.extractLatestDeltas) {
				ret :+= mod
			}
		}
		ret
	}
	final def applyLatestDelta (delta : NetworkedWorldDataUpdate) : Boolean = {
		delta match {
			case nu : NetworkedWorldDataUpdate if nu.fieldId < subHandlers.size => subHandlers(nu.fieldId).applyLatestDelta(nu)
			case _ => applyLatestDeltaImpl(delta)
		}
	}

	def extractLatestDeltasImpl : Seq[NetworkedWorldDataUpdate] = Seq()
	def applyLatestDeltaImpl (delta : NetworkedWorldDataUpdate) : Boolean = false

	def watchGrid[T : Manifest,U <: TLoggingTalea[T]] (grid : GenericTaleaGrid[T,U] with TGridWithChangeQueue[T,U]): Unit = {
		subHandlers :+= new GridDeltaHandler[T,U](grid,subHandlers.size)
	}

	def addHandler(handler : TNetworkedDataDeltaHandler): Unit = {
		handler.fieldId = subHandlers.size
		subHandlers :+= handler
	}

}


trait TNetworkedDataDeltaExtractor {
	def extractLatestDeltas : Seq[NetworkedWorldDataUpdate]
}

trait TNetworkedDataDeltaApplicator {
	def applyLatestDelta (delta : NetworkedWorldDataUpdate) : Boolean
}

trait TNetworkedDataDeltaHandler extends TNetworkedDataDeltaExtractor with TNetworkedDataDeltaApplicator {
	var fieldId : Int = _
}