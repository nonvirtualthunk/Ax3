package arx.bol.game.graphics.data

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.bol.game.entities.data.InventorySlot
import arx.core.Moddable
import arx.core.math.Recti
import arx.core.vec.ReadVec4f
import arx.core.vec.Vec2f
import arx.core.vec.Vec2i
import arx.engine.graphics.data.TGraphicsData
import arx.graphics.GL
import arx.graphics.pov.TopDownCamera

import scalaxy.loops._

class BagOfLegendGraphicsData extends TGraphicsData {
	val pov = new TopDownCamera(30)

	var slotHighlights = Map[String, SlotHighlight]()

	var viewport = Moddable(() => GL.viewport)

	var leftGrabSlotOffset = Vec2i.Zero
	var leftGrabExactOffset = Vec2f.Zero
}

case class SlotHighlight(slots : Traversable[InventorySlot], color : ReadVec4f)
