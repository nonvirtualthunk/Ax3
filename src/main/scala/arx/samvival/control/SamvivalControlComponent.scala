package arx.samvival.control

import arx.core.math.Recti
import arx.core.vec.{ReadVec2f, Vec2f, Vec3f}
import arx.core.vec.coordinates.{AxialVec, AxialVec3}
import arx.engine.advanced.lenginecomponents.LControlComponent
import arx.engine.graphics.data.PovData
import arx.graphics.GL
import arx.samvival.graphics.components.SamvivalGraphicsComponent
import arx.samvival.graphics.components.SamvivalGraphicsComponent.HexSize

trait SamvivalControlComponent extends LControlComponent {
	def pov = graphics[PovData].pov
	def pixelToHex(pixel : ReadVec2f, viewport : Recti = GL.viewport) = {
		val unprojected = pov.unproject(Vec3f(pixel,0.0f), viewport)
		AxialVec.fromCartesian(unprojected.xy, SamvivalGraphicsComponent.HexSize).withLayer(0)
	}
	def hexToPixel(hex : AxialVec, viewport : Recti) = {
		val px = hex.cartesianX(HexSize)
		val py = hex.cartesianY(HexSize)

		Vec2f(px + pov.eye.x + (viewport.width >> 1), py + pov.eye.y + (viewport.height >> 1))
	}
}
