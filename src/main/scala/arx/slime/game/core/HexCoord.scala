package arx.slime.game.core

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.vec.ReadVec2i
import arx.core.vec.Vec2f

case class HexCoord(r : Int, q : Int) {
	def toCartesian = Vec2f(0.5f * 1.5f * r, 0.5f * sqrtf(3.0f) * (q + r * 0.5f))

	def - (h : HexCoord) = HexCoord(this.r - h.r,this.q - h.q)
	def + (h : HexCoord) = HexCoord(this.r + h.r,this.q + h.q)
}

object HexCoord {
	def apply(v : ReadVec2i) : HexCoord = HexCoord(v.x,v.y)
}