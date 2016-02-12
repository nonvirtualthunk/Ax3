package arx.tmp.game.logic.datastructures.voxelregions

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/23/15
 * Time: 1:19 PM
 */

import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.DifferenceInfiniteVoxelView
import arx.tmp.game.logic.datastructures.TInfiniteVoxelView

class NegatingVoxelRegion protected[datastructures] (val baseRegion : VoxelRegion, minus : VoxelRegion) extends VoxelRegion {
	lazy val filteredVoxels = baseRegion.filter((v : VoxelCoord) => !minus.contains(v))

	override def performIntersection(other: VoxelRegion): VoxelRegion = baseRegion.intersect(other).without(minus)
	override def boundingRegion: VoxelRegion = baseRegion.boundingRegion
	override def asVoxelView: TInfiniteVoxelView[Byte] = new DifferenceInfiniteVoxelView[Byte](baseRegion.asVoxelView,minus.asVoxelView,0.toByte)
	lazy val max: VoxelCoord = if (! minus.contains(baseRegion.max)) { baseRegion.max } else { var m = filteredVoxels.head; filteredVoxels.foreach(v => m = m.min(v)); m }
	lazy val min: VoxelCoord = if (! minus.contains(baseRegion.min)) { baseRegion.min } else { var m = filteredVoxels.head; filteredVoxels.foreach(v => m = m.max(v)); m }
	lazy val edgeVoxels: VoxelRegion = {
		// Remove all base edge voxels that have been removed, add all base voxels that are adjacent to the removal region
		(baseRegion.edgeVoxels without minus) union (minus.adjacentVoxels intersect baseRegion)
	}
	lazy val adjacentVoxels : VoxelRegion = {
		val baseAdjacents = baseRegion.adjacentVoxels
		var removedAdjacents = Vector[VoxelCoord]()
		baseAdjacents.foreachUnsafe(v => {
			if (VoxelCoord.forAllAdjacentUnsafe(v,(adj) => ! baseRegion.contains(adj))) {
				removedAdjacents :+= v
			}
		})

		val possibleNewAdjacents = (baseRegion intersect minus)
		var newAdjacents = Vector[VoxelCoord]()
		possibleNewAdjacents.foreachUnsafe(v => {
			if (VoxelCoord.forAnyAdjacentUnsafe(v,(adj) => this.contains(adj))) {
				newAdjacents :+= v
			}
		})
		
		// Remove all old adjacents that are no longer connected to anything, add all voxels that were cut out, that still have a valid neighbor
		(baseAdjacents without VoxelRegion(removedAdjacents)) union VoxelRegion(newAdjacents)
	}
	override def contains(x: Int, y: Int, z: Int): Boolean = baseRegion.contains(x,y,z) && !minus.contains(x,y,z)
	override def foreachUnsafe[U](f: (VoxelCoord) => U): Unit = baseRegion.foreachUnsafe(v => if (!minus.contains(v)) {f(v)})
	override def foreach[U](f: (VoxelCoord) => U): Unit = baseRegion.foreach(v => if (!minus.contains(v)) {f(v)})

	override def toString(): String = s"VoxelRegion($baseRegion without $minus)"
	override def size: Int = baseRegion.size - minus.size
	override def isEmpty = size == 0
}
