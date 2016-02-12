package arx.axistential.graphics.camera

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/13/15
 * Time: 8:16 AM
 */

import arx.axistential.game.data.world.TerrainData
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.ReadVec3f
import arx.core.vec.Vec3f
import arx.engine.world.World

import scala.language.postfixOps
import scalaxy.loops._

class TerrainAwareCamera(world: World) extends AnthologiconCamera {
	lazy val TD = world.aux[TerrainData]

	def acceptableLocation(pos : ReadVec3f) = {
		val basePoint = ObjectCoord(pos).toVoxelCoord
		var unacceptable = TD.isOccupied(basePoint)
		for (q <- 0 until 6 optimized) {
			if (! unacceptable && TD.isOccupied(basePoint + dirvec(q))) {
				unacceptable = true
			}
		}
		!unacceptable
	}

	override def update(f: Float): Unit = {
		val lastEye = ReadVec3f(eye)
//		val lastAngles = ReadVec2f(angles)

		val startedBad = !acceptableLocation(eye)

		super.update (f)

		val newEye = ReadVec3f(eye)

		// if it was bad to begin with, doesn't matter, allow anything, we want to be able to un-embed
		if (!startedBad) {
			// otherwise, if our new location is unacceptable, figure out how to revert
			val newBad = !acceptableLocation(eye)
			if (newBad) {
				// jump back to the old eye position and attempt to move along each axis individually
				// discarding any that put us into a bad position
				var replaceEye = lastEye
				for (axis <- 0 until 3 optimized) {
					val tmp = Vec3f(replaceEye)
					tmp(axis) = newEye(axis)
					if (acceptableLocation(tmp)) {
						replaceEye = tmp
					}
				}
				eye = replaceEye
			}
		}
	}
}
