package arx.rog2.game.data.entity

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.vec.Cardinals
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.data.TGameEntityAuxData
import arx.engine.entity.TGameEntity
import arx.graphics.helpers.HSBA

import scalaxy.loops.rangeExtensions

class Physical extends TGameEntityAuxData {
	var position = VoxelCoord.Center
	var color = HSBA(0.0f,0.0f,1.0f,1.0f)
	var dimensions = 1.voxel x 1.voxel x 1.voxel
	var facing = Cardinals.Left

	var effectiveOpacity = 0.5f
	var solid = true
	var heldIn = none[TGameEntity]

	var drawOrder = 0

	var drawInfo : DrawInfo = TextureDrawInfo("unknown")

	/**
	  * x/y/z given in voxel coordinates
	  */
	def forEveryVoxelInShape(f : (Int,Int,Int) => Unit): Unit = {
		for (x <- position.x until position.x + dimensions.x.inVoxels.toInt optimized;
			  y <- position.y until position.y + dimensions.y.inVoxels.toInt optimized;
			  z <- position.z until position.z + dimensions.z.inVoxels.toInt optimized) {
			f(x,y,z)
		}
	}
}
