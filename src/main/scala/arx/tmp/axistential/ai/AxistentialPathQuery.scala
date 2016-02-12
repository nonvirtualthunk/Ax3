package arx.axistential.ai

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/6/13
 * Time: 10:55 AM
 * To change this template use File | Settings | File Templates.
 */

import arx.application.Noto
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.core.vec.ReadVec3i
import arx.core.vec.coordinates.MutableObjectCoord
import arx.core.vec.coordinates.MutableVoxelCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.voxelregions.VoxelRegion

case class AxistentialPathQuery (
  world : World,
  from : VoxelCoord,
  to : VoxelRegion,
  voxelDimensions : ReadVec3i,
  maxCost : Int,
  moveCostFunction : (VoxelCoord,VoxelCoord,Int,Int) => Float,
  searchLimitFunction : (VoxelCoord) => Boolean,
  obstructionFunction : (VoxelCoord) => Boolean,
  isSupportedFunction : (VoxelCoord) => Boolean,
  heuristicFunction : (VoxelCoord,VoxelCoord) => Float
) {
	var floodQuery = false
	var finishedCriteria : (VoxelCoord) => Boolean = (v) => false
	var allowUnsupportedEndpoint = false
}

object AxistentialPathQuery {

	@inline
	def defaultHeuristic ( from : VoxelCoord, to : VoxelCoord ) = //absf(to.x - from.x) + absf(to.y - from.y) + absf(to.z - from.z)
		sqrtf( (from.x-to.x)*(from.x-to.x) + (from.y-to.y)*(from.y-to.y) ) + absf(from.z-to.z)


	import arx.Prelude._
	val unembedAxisOrder = Array(Up,Left,Front,Right,Back,Down)
	val maxUnembed = 0.2f
	val unembedStep = 0.025f

	def apply (
		 entity : TPhysicalEntity,
		 to : VoxelRegion,
		 maxCost : Int,
		 moveCostFunction : (VoxelCoord,VoxelCoord,Int,Int) => Float,
		 searchLimitFunction : (VoxelCoord) => Boolean,
		 obstructionFunction : (VoxelCoord) => Boolean,
		 isSupportedFunction : (VoxelCoord) => Boolean,
		 heuristicFunction : (VoxelCoord,VoxelCoord) => Float = AxistentialPathQuery.defaultHeuristic) : AxistentialPathQuery =
	{
		val rawPos = entity.adjustedFootPos

		// This section has us plan from where we expect to be one tick from now, may or may not be wise
		val velAdjustedPos = rawPos + (entity.velocity * 0.0167.seconds).inVoxels
		val velAdjustedVoxPos = velAdjustedPos.toVoxelCoord

		val basePos = if (obstructionFunction(velAdjustedVoxPos)) {
			val mutOC = MutableObjectCoord(velAdjustedPos)
			val vc = MutableVoxelCoord(0,0,0)
			var offset = unembedStep
			var continue = true

			while (offset < maxUnembed && continue) {
				var i = 0
				while (i < unembedAxisOrder.length && continue) {
					val axis = unembedAxisOrder(i)
					mutOC(axis) = velAdjustedPos(axis) + offset

					if (!obstructionFunction(mutOC.toVoxelCoord(vc))) {
						continue = false
					} else {
						mutOC(axis) = velAdjustedPos(axis) // reset that axis
					}

					i += 1
				}
				offset += unembedStep
			}

			if (! continue) {
				mutOC
			} else {
				Noto.warn("Could not find unembeded location to search from")
				velAdjustedPos
			}
		} else {
			velAdjustedPos
		}

		val baseVoxelPos = basePos.toVoxelCoord


//		val startPos = baseVoxelPos

		AxistentialPathQuery (
			entity.world,
			baseVoxelPos,
			to,
			entity.boundingDimensions.inVoxels.round,
			maxCost,
			moveCostFunction,
			searchLimitFunction,
			obstructionFunction,
			isSupportedFunction,
			heuristicFunction
		)

	}
}

object AxistentialAllPathsQuery {
	/**
	 * search limit function - treats as forbidden any voxel for which it returns false
	 */
	def apply (
		 entity : TPhysicalEntity,
		 maxCost : Int,
		 moveCostFunction : (VoxelCoord,VoxelCoord,Int,Int) => Float,
		 searchLimitFunction : (VoxelCoord) => Boolean,
		 obstructionFunction : (VoxelCoord) => Boolean,
		 isSupportedFunction : (VoxelCoord) => Boolean) =
	{
		val ret = AxistentialPathQuery(
			entity.world,
			entity.adjustedFootPos.toVoxelCoord,
			VoxelRegion(VoxelCoord.Center),
			entity.boundingDimensions.inVoxels.round,
			maxCost,
			moveCostFunction,
			searchLimitFunction,
			obstructionFunction,
			isSupportedFunction,
			(_,_) => 0.0f
		)

		ret.floodQuery = true
		ret
	}
}

object AxistentialFloodSearchQuery {

	def apply (
				 entity : TPhysicalEntity,
				 targetDesirability : Float,
				 maxCost : Int,
				 moveCostFunction : (VoxelCoord,VoxelCoord,Int,Int) => Float,
				 desirabilityFunction : (VoxelCoord) => Float,
				 obstructionFunction : (VoxelCoord) => Boolean,
				 isSupportedFunction : (VoxelCoord) => Boolean,
				 searchLimitFunction : (VoxelCoord) => Boolean = (v) => true
	 ) = {
		val ret = AxistentialPathQuery(
			entity.world,
			entity.adjustedFootPos.toVoxelCoord,
			VoxelRegion(VoxelCoord.Center),
			entity.boundingDimensions.inVoxels.round,
			maxCost,
			moveCostFunction,
			searchLimitFunction,
			obstructionFunction,
			isSupportedFunction,
			(v,_) => targetDesirability - desirabilityFunction(v)
		)
		ret.floodQuery = true
		ret.finishedCriteria = (v) => desirabilityFunction(v) >= targetDesirability
		ret
	}
}