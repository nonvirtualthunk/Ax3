package arx.tmp.game.networking

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/24/15
 * Time: 8:41 PM
 */

import arx.tmp.game.logic.datastructures.GenericTaleaGrid
import arx.tmp.game.logic.datastructures.TGridWithChangeQueue
import arx.tmp.game.logic.datastructures.TLoggingTalea
import arx.tmp.game.logic.world.data.NetworkedWorldDataUpdate

class GridDeltaHandler[T : Manifest,U <: TLoggingTalea[T]] (grid : GenericTaleaGrid[T, U] with TGridWithChangeQueue[T,U], gridId : Int)
	extends TNetworkedDataDeltaHandler
{


	val consumer = grid.changedTaleae.createConsumer
	for (tal <- grid.allTaleae) {
		consumer.updateSubOffsetFor(tal,tal.modifiedCount)
	}


	override def extractLatestDeltas: Seq[NetworkedWorldDataUpdate] = {
		var deltas = List[GridDelta[T]]()
		for (t <- consumer.dequeueAll()) {

			val subOffset = consumer.subOffsetFor(t)
			val modifications = t.loggedModifications.takeWhile(m => m.revision > subOffset)
			if (modifications.nonEmpty) {
				consumer.updateSubOffsetFor(t,modifications.last.revision)
				deltas ::= GridDelta(gridId,t.position,modifications)
			}
		}

		deltas
	}

	override def applyLatestDelta(untypedDelta: NetworkedWorldDataUpdate): Boolean = {
		untypedDelta match {
			case tdelta : GridDelta[T] =>
				val taleaPos = tdelta.taleaPosition
				val window = grid.windowCenteredOnTaleaAt(taleaPos,readOnly = false)
				grid.modificationBlock(window) {
					for (mod <- tdelta.modifications) {
						window.centerTalea(mod.position.x,mod.position.y,mod.position.z) = mod.newValue
					}
				}
				true
			case _ => false
		}
	}
}

