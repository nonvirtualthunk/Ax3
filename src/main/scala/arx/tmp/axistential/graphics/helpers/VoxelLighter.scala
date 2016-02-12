package arx.axistential.graphics.helpers

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/31/14
 * Time: 3:56 PM
 */

import arx.axistential.game.data.world.TerrainData
import arx.core.vec.Vec3f
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.world.World
import arx.tmp.game.logic.world.data.LightData

import scalaxy.loops._

class VoxelLighter(world:World) {
	private final val TD = world.aux[TerrainData]
	private final val LD = world.aux[LightData]

	def lightOnVoxelPoint (vox: VoxelCoord,face : Int,k : Int) = {
		val q = face
		val ax = vox.x + dirvec(face).x
		val ay = vox.y + dirvec(face).y
		val az = vox.z + dirvec(face).z

		val terrainView = TD.materialGrid
		val lightView = LD.globalLighting(0)
		val localLightData = LD.localLightingDefinedAt(vox)
		val nLightChannels = localLightData.length
		val avgLocalLight = Array.ofDim[Float](nLightChannels)
		val colorHolder = Vec3f(0.0f)

		var globalLightStrength = 0.0f

		var counter = 0.0f
		var iaxis = 0x0
		while ( iaxis >= -1 ) {
			var jaxis = 0x0
			while ( jaxis >= -1 ) {
				val lx = ax + (cubeOrthos(q)(k)(0).x & iaxis) + (cubeOrthos(q)(k)(1).x & jaxis)
				val ly = ay + (cubeOrthos(q)(k)(0).y & iaxis) + (cubeOrthos(q)(k)(1).y & jaxis)
				val lz = az + (cubeOrthos(q)(k)(0).z & iaxis) + (cubeOrthos(q)(k)(1).z & jaxis)

				if ( terrainView(lx,ly,lz) <= 0.toByte ) {
					val rawLightByte = lightView(lx,ly,lz)
					globalLightStrength += CommonRendering.lightMults(math.max(rawLightByte,0))
					var lli = 0
					while ( lli < nLightChannels ) {
						avgLocalLight(lli) += CommonRendering.localLightMults(localLightData(lli).window(lx,ly,lz))
						lli += 1}
					counter += 1.0f
				} else {
					counter += 0.3f
				}
				jaxis += 0xffffffff }
			iaxis += 0xffffffff }

		var localLightStrength = 0.0f
		if ( counter > 0.0f ) {
			globalLightStrength /= counter

			if ( nLightChannels > 0 ) {
				colorHolder.r = 0.0f; colorHolder.g = 0.0f; colorHolder.b = 0.0f
				var lightSum = 0.000001f

				var lli = 0
				while ( lli < nLightChannels ) {
					avgLocalLight(lli) /= counter
					localLightStrength = math.max(localLightStrength,avgLocalLight(lli))
					lightSum += avgLocalLight(lli)
					colorHolder.r += localLightData(lli).color.r * avgLocalLight(lli)
					colorHolder.g += localLightData(lli).color.g * avgLocalLight(lli)
					colorHolder.b += localLightData(lli).color.b * avgLocalLight(lli)
					lli += 1
				}

				if ( lightSum > localLightStrength && localLightStrength < 0.85f ) {
					localLightStrength = math.min(0.85f,localLightStrength + (lightSum - localLightStrength) * 0.75f)
				}
				colorHolder.r /= lightSum
				colorHolder.g /= lightSum
				colorHolder.b /= lightSum
				//															colorHolder.r = 1.0f; colorHolder.g = 1.0f; colorHolder.b = 1.0f
			} else { //if we don't have locals, the light color will just be global light's color
				colorHolder.r = 1.0f; colorHolder.g = 1.0f; colorHolder.b = 1.0f
			}
		} else {
			colorHolder.r = 0.0f; colorHolder.g = 0.0f; colorHolder.b = 0.0f
		}

		for (lli <- 0 until nLightChannels optimized) {
			avgLocalLight(lli) = 0.0f
		}

		CommonRendering.LightResult(globalLightStrength,localLightStrength,colorHolder)
	}
}
