package arx.slime.graphics.data

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.vec._
import arx.engine.graphics.data.TGraphicsData
import arx.graphics.Image
import arx.resource.ResourceManager

import scalaxy.loops._

class UIData extends TGraphicsData {

}


class GraphicsBlock {
	 var quads = List[Quad]()
}


case class Quad (
	var position : ReadVec3f = Vec3f.Zero,
	var forward : ReadVec3f = Vec3f.UnitX,
	var ortho : ReadVec3f = Vec3f.UnitY,
	var dimensions : ReadVec2f = Vec2f.One,
	var color : ReadVec4f = Vec4f.One,
	var image : Image = ResourceManager.defaultImage)

object Quad {
	val SentinelTexCoords = Array(Vec2f.Zero,Vec2f.Zero,Vec2f.Zero,Vec2f.Zero)
}