package arx.samvival.game.logic

import arx.core.vec.coordinates.AxialVec
import arx.engine.lworld.LWorld
import arx.samvival.game.entities._
import arx.samvival.game.events.GameEvents

object TerrainGenerator {
	def generate(world : LWorld): Unit = {
		for (x <- -20 until 20; y <- -20 until 20) {
			val pos = AxialVec(x,y)
			val tileEnt = world.createEntity(Tiles.tileEntityId(x,y))

			val tile = new Tile()
			tile.position = pos

			val terrain : Terrain = new Terrain()
			terrain.kind = Taxonomy.Terrains.Flatland

			val vegetation = new Vegetation()
			vegetation.kind = Taxonomy.Vegetations.Grassland

			{
				world.attachData[Tile](tileEnt, tile)
				world.attachData(tileEnt, terrain)
				world.attachData(tileEnt, vegetation)
			}
		}

		world.addEvent(GameEvents.TerrainGenerated)
	}
}
