package arx.tmp.game.logic.world.data

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/7/16
  * Time: 2:24 PM
  */

import arx.Prelude._
import arx.engine.data.TWorldAuxData
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.world.SpatialRegion
import scalaxy.loops._
import arx.core.vec._

class VoxelWorldData extends TWorldAuxData {

	val worldRegion = SpatialRegion.fromCorners(VoxelCoord.Center - 1024, VoxelCoord.Center + 1024)
}
