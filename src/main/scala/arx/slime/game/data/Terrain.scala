package arx.slime.game.data

/**
  * TODO: Add javadoc
  */

import arx.core.datastructures.FiniteGrid2D
import arx.core.vec.Vec2i
import arx.engine.data.TWorldAuxData
import arx.slime.game.archetypes.NoTerrain
import arx.slime.game.archetypes.TerrainType
import arx.slime.game.core.HexCoord

class Terrain extends TWorldAuxData {
	val radius = 25
	protected val hexTerrains = FiniteGrid2D.apply(Vec2i(radius*2+1,radius*2+1), NoTerrain : TerrainType)
	def centerCoord = HexCoord(radius,radius)

	def apply(hc : HexCoord) = hexTerrains(hc.q + radius, hc.r + radius)
	def update(hc : HexCoord, t : TerrainType): Unit = {
		hexTerrains(hc.q + radius, hc.r + radius) = t
	}
}
