package arx.axistential.game.components.construction

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/8/14
 * Time: 1:54 PM
 */

import arx.application.Noto
import arx.axistential.game.data.world.TerrainData
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.TaleaGrid.TaleaModificationsCompletedEvent
import arx.tmp.game.logic.datastructures.LoggedTaleaModification
import arx.tmp.game.logic.datastructures.TLoggingTalea
import arx.tmp.game.logic.datastructures.TaleaGrid
import com.carrotsearch.hppc.LongLongOpenHashMap

/**
 * !!!NOTE!!!
 * This class does not appear to have any uses, was likely created back when we were first thinking about doing
 * structural support and the like, but we ended up going a different route. This is preserved purely because it has
 * a handy isolated code section for performing some process on a terrain block being changed, which is broadly useful
 */
class ConstructionGameComponent extends GameEngineComponent {

	val lastUpdatedAt = new LongLongOpenHashMap()



	override def initialize(): Unit = {
		world.aux[TerrainData].materialGrid.onEvent {
			case TaleaModificationsCompletedEvent(taleae) => {
				var changes = List[LoggedTaleaModification[Byte]]()
				for ( talea <- taleae ) {
					talea match {
						case lt : TLoggingTalea[Byte] => {
							val hash = VoxelCoord.hashL( talea.x, talea.y, talea.z )
							val lastUpdated = if ( lastUpdatedAt.containsKey( hash ) ) {
								lastUpdatedAt.lget()
							} else {
								-1L
							}
							lastUpdatedAt.put(hash,talea.modifiedCount)
							val mods = lt.loggedModifications.takeWhile( m => m.revision > lastUpdated )
							changes :::= mods
						}
						case _ => Noto.warn("We really expected terrain data to have logging taleae")
					}
				}

				changesMade(changes)
			}
		}
	}

	def changesMade(modifications: List[LoggedTaleaModification[Byte]]) {
		val TD = world.aux[TerrainData]

		for ( m <- modifications if  ( m.oldValue <= 0 && m.newValue > 0 ) || ( m.oldValue > 0 && m.newValue <= 0) ) {
			val newMat = TD.materialMapping( m.newValue )


		}
	}


	def update(time: UnitOfTime): Unit = {

	}
}
