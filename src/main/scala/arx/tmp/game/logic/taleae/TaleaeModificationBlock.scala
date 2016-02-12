package arx.tmp.game.logic.taleae

import arx.tmp.game.logic.datastructures.TaleaGrid.TaleaModificationsCompletedEvent
import arx.tmp.game.logic.datastructures.ITalea
import arx.tmp.game.logic.datastructures.TTaleaGrid
import arx.tmp.game.logic.datastructures.Talea
import arx.tmp.game.logic.datastructures.TaleaGrid

/**
 *
 */

object TaleaeModificationBlock {
	def apply ( grid : TTaleaGrid[_,_], taleae_iter : Iterable[ITalea[_]] , fire : Boolean = true )( stmt : => Unit ) : Set[ITalea[_]] = {
		val taleae = taleae_iter.toIndexedSeq
		val preMods = taleae.map( t => (t.modifiedCount,t.edgeModifiedCount.clone()) )

		stmt

		val postMods = taleae.map( t => (t.modifiedCount,t.edgeModifiedCount.clone()) )

		var preModMap = Map[ITalea[_],Long]()
		var altered = Set[ITalea[_]]()
		var i = 0; while ( i < postMods.size ) {
			if ( postMods(i)._1 > preMods(i)._1 ) {
				altered += taleae(i)
				preModMap += (taleae(i) -> preMods(i)._1)
				var j = 0; while ( j < 6 ) {
					if ( postMods(i)._2(j) > preMods(i)._2(j) ) {
						var shouldBreak = false
						var k = 0; while ( k < taleae.size && ! shouldBreak ) {
							val t = taleae(k)
							if ( t.position == taleae(i).position + Talea.cardinals(j) ) {
								altered += t
								preModMap += (t -> preMods(k)._1)
								shouldBreak = true
							}
						k += 1}

						if ( ! shouldBreak && grid != null) {
							val toAdd = grid.rawGetTalea(taleae(i).position + Talea.cardinals(j),readOnly = true).asInstanceOf[ITalea[_]]
							altered += toAdd
							preModMap += (toAdd -> toAdd.modifiedCount)
						}
					}
					j += 1
				}
			}
			i += 1
		}

		if ( grid != null && fire ) {
			val event = TaleaModificationsCompletedEvent(altered)
			event.preModificationCounts = preModMap
			grid.fireEvent(event)
		}
		altered
	}
}