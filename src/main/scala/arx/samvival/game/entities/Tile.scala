package arx.samvival.game.entities

import arx.core.macros.GenerateCompanion
import arx.core.math.Sext
import arx.core.vec.coordinates.{AxialVec, AxialVec3}
import arx.engine.data.TAuxData
import arx.engine.entity.Taxon
import arx.engine.lworld.{LEntity, LWorld, LWorldView}

@GenerateCompanion
class Tile extends SVAuxData {
	var entities = Set[LEntity]()
	var position = AxialVec.Zero
}

object Tiles {
	def tileAt(v : AxialVec, z : Int = 0) : LEntity = new LEntity(tileEntityId(v.q,v.r,z))
	def tileAt(v : AxialVec3) : LEntity = new LEntity(tileEntityId(v.q, v.r, v.l))
	def tileAt(q : Int, r : Int, z : Int) : LEntity = new LEntity(tileEntityId(q,r,z))
	def tileAt(q : Int, r : Int) : LEntity = new LEntity(tileEntityId(q,r,0))

	// 32 z levels, 4096 y, 4096 x
	// 5 bits, 12 bits, 12 bits
	/** Compute the entity id to use for the tile at the given x/y/(z) coordinates */
	def tileEntityId(x : Int, y : Int, z : Int = 0) = (1 << 32L) + ((z + 16) << 17) + ((y + 2048) << 12) + (x + 2048)

	def characterOnTile(v : AxialVec3)(implicit world : LWorldView) : Option[LEntity] = {
		val tileEnt = tileAt(v)
		val tile = tileEnt[Tile]
		tile.entities.find(e => {
			e.hasData[CharacterInfo]
		})
	}
}

@GenerateCompanion
class Terrain extends SVAuxData {
	var fertility = 0
	var cover = Sext(0)
	var elevation = 0
	var moveCost = Sext(1)
	var kind : Taxon = Taxonomy.UnknownThing
}

@GenerateCompanion
class Vegetation extends SVAuxData {
	var cover = Sext(0)
	var moveCost = Sext(0)
	var kind : Taxon = Taxonomy.UnknownThing
}
