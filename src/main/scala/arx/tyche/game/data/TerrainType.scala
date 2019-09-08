package arx.tyche.game.data

import arx.engine.entity.{HasIdentity, Identity, Taxon}

sealed abstract class TerrainType(name : String, parents : Taxon*) extends HasIdentity {
	val identity = Identity(Taxon(name, Taxonomy.TerrainType) :: parents.toList)
}

object TerrainType {
	import Taxonomy.Terrain._

	case object Desolation extends TerrainType("desolation")

	case object Grassland extends TerrainType("grassland")

	case object Swamp extends TerrainType("swamp", WetTerrain)

	case object Ocean extends TerrainType("ocean", WetTerrain)

	case object Desert extends TerrainType("desert")

	case object Mountain extends TerrainType("mountain")
}

