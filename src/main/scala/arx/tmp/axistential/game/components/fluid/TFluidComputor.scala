package arx.axistential.game.components.fluid

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/11/13
 * Time: 2:17 PM
 */


import arx.core.datastructures.TBareBonesSet
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.world.World

trait TFluidComputor {
	/** Advance the fluid simulation forward one step
	  *
	  * @param env the environment to update
	  * @param dirtyVoxels which voxels are considered "dirty" and in need of update
	  * @return list of voxels which should be considered "dirty" for the next timestep
	  */
	def stepFluidSimulation ( env : World , dirtyVoxels : TBareBonesSet[VoxelCoord] ) : TBareBonesSet[VoxelCoord]

	def settleInitialSimulation ( world : World )

	def voxelsRemoved ( env : World , voxels : Traversable[VoxelCoord] ) {}

	def isPull = true

	var pressureEnabled = true
}