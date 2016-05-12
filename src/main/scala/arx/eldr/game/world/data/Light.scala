package arx.eldr.game.world.data

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 3/7/16
  * Time: 7:52 AM
  */

import arx.Prelude._
import arx.core.datastructures.voxel.VoxelGrid
import arx.engine.data.TWorldAuxData
import scalaxy.loops._
import arx.core.vec._

class Light extends TWorldAuxData {
	val MaxLight = 127

	var global = Array(new VoxelGrid(MaxLight.toByte))
	var globalColor = Array(Vec4f(1.1f,0.9f,0.8f,1.0f))
}
