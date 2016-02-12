package arx.axistential.game.logic.general

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/5/14
 * Time: 9:39 AM
 */

import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.data.world.TopologicalData
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.voxelregions.VoxelRegion

object TerrainLogic {

	/**
	 * Works from the TopologicalData to find the first voxel in the column -- starting from the topological height and
	 * working downward -- that is solid. When found, it returns the VoxelCoord immediately above the solid voxel.
	 */
	def oneAboveFirstTerrainInColumn (x:Int,y:Int, TD : TerrainData, PD : TopologicalData ) : VoxelCoord = {
		val h = PD.heightmap(x,y)
		val d = PD.depthmap(x,y)
		if ( d < h ) {
			var z = VoxelCoord.Center.z + h
			while ( z >= VoxelCoord.Center.z + h - 128 ) {
				if ( TD.materialByteAt(x,y,z-1) > 0 ) { return VoxelCoord(x,y,z) }
				z -= 1
			}
			VoxelCoord.Sentinel
		} else {
			VoxelCoord.Sentinel
		}
	}

	def areAnySolid (inRegion : VoxelRegion)(implicit TD : TerrainData) = {
		inRegion.existsUnsafe(v => TD.isSolid(v))
	}
}
