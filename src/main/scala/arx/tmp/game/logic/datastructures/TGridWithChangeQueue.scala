package arx.tmp.game.logic.datastructures

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/17/15
 * Time: 8:41 PM
 */

import arx.core.datastructures.BiLevelMultiqueue
import arx.tmp.game.logic.datastructures.TaleaGrid.TaleaModificationsCompletedEvent

trait TGridWithChangeQueue [T,TaleaType <: ITalea[T]] extends TTaleaGrid[T,TaleaType] {
	val changedTaleae = new BiLevelMultiqueue[TaleaType]

	onEvent {
		case TaleaModificationsCompletedEvent(taleae) => {
			// taleae will always be an iterable of the appropriate talea type, since we are listening
			// for them from ourselves
			changedTaleae.enqueue(taleae.asInstanceOf[Iterable[TaleaType]])
		}
	}
}