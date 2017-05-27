package arx.rog2.game.world

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.datastructures.voxelregions.voxelregions.EmptyVoxelRegion
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.data.TGameEntityAuxData
import arx.engine.data.TWorldAuxData
import arx.engine.world.World

import scalaxy.loops._


class RoomData extends TWorldAuxData {
	var rooms = List[Room]()
}

class RoomConnection (from : Room, to : Room) {

}

class Room {
	val region : VoxelRegion = VoxelRegion.empty
}

class DungeonGenerator {
	def generate(world: World): Unit = {
		val RD = world[RoomData]


	}
}

trait RoomGenerator {
	def generate(world : World, otherRooms : List[Room])
}