package arx.rog2.game.data.entity

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.data.TGameEntityAuxData
import arx.engine.entity.TGameEntity
import arx.graphics.helpers.HSBA
import arx.modules.lighting.OctantShadowGrid
import arx.rog2.game.data.entity.DrawHeight.Center

import scalaxy.loops.rangeExtensions

class Creature extends TGameEntityAuxData {
	var maxHP = 10
	var damageTaken = 0
	def hp = maxHP - damageTaken

	var maxFood = 50
	var hunger = 0.0f
	var metabolism = 0.025f // per second
	def food = maxFood - hunger.toInt

	var maxSanity = 10
	var sanityLost = 0
	def sanity = maxSanity - sanityLost

	var damageDealt = 3

	var sightRange = 30.voxels

	var actionTime = 0.seconds

	var visionGrid = new OctantShadowGrid
}




sealed trait DrawHeight
object DrawHeight {
	case object Floor extends DrawHeight
	case object Center extends DrawHeight
}

sealed trait DrawInfo

case class TextureDrawInfo(texture : String, drawHeight : DrawHeight = DrawHeight.Center) extends DrawInfo {

}
case class SliceDrawInfo(texture : String, textureRotation : Int = 0) extends DrawInfo {
}
case class CubeDrawInfo(texture : String) extends DrawInfo {

}
