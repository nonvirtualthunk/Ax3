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
	var globalFullLight = Array(32.byteValue)
	var global = Array(new VoxelGrid(globalFullLight(0)))
	var globalColor = Array(Vec3f(1.00f,0.95f,0.85f))
	var globalLightStrength = Array(1.0f)

	var lowSkyColor = Vec3f(1,1,1)
	var highSkyColor = Vec3f(0.61f,0.87f,1.0f)
}
