package arx.eldr.game.events

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.vec.coordinates.TaleaCoord
import arx.engine.control.event.Event.Event
import scalaxy.loops._

class TerrainModifiedEvent extends Event {
	var modifiedRegions = List[VoxelRegion]()
	var preModificationRevisions = Map[TaleaCoord,Int]()
}