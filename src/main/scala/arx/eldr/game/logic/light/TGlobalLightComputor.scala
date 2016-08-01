package arx.eldr.game.logic.light

import arx.core.datastructures.voxel.Talea
import arx.core.vec.Cardinals._
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.world.World


/**
 *
 */

abstract class TGlobalLightComputor(worldArg: World) extends TLightComputor(worldArg) {
	val minimumZ = -Talea.dimension * 2
	val absoluteMinimumZ = -Talea.dimension * 5
	val absoluteMaximumZ = Talea.dimension * 5


	def preInitializeLighting(primaryQ: Int, gli: Int) {}

	def preInitializeLightingForTalea(loc: VoxelCoord, primaryQ: Int, gli: Int)

	def initializeLightingForTalea(loc: VoxelCoord, primaryQ: Int, gli: Int)

	def updateLightingForTalea(loc: VoxelCoord,
										primaryQ: Int, gli: Int,
										localLighting: Boolean = false): Set[VoxelCoord]


	def highestAdjacentLightValue(x: Int, y: Int, z: Int,
											gli: Int,
											fullLight: Byte): Byte = {
		var maxValue = 0
		var counter = 0
		while (counter < 6) {
			val curValue = lightData.global(gli)(x + cardinals(counter).x, y + cardinals(counter).y, z + cardinals(counter).z)
			maxValue = math.max(maxValue, if (curValue == fullLight && counter == Top) {
				curValue
			} else {
				curValue - 1
			})
			counter += 1
		}

		val transp = terrainData.materialAt(x, y, z).byteTransparency
		if (transp <= 0) {
			(maxValue + transp).toByte
		} else {
			maxValue.toByte
		}
	}
}