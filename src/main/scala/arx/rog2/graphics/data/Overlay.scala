package arx.rog2.graphics.data

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 6/4/17
  * Time: 6:39 PM
  */

import arx.Prelude._
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion

import scalaxy.loops._
import arx.core.vec._
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.graphics.data.TGraphicsData
import arx.graphics.TToImage
import arx.graphics.helpers.HSBA

class Overlay extends TGraphicsData {
	var overlaidRegions = Map[AnyRef, OverlayRegion]()
}


case class OverlayRegion(voxel : VoxelRegion, image : TToImage, color : HSBA)