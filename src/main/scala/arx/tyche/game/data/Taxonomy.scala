package arx.tyche.game.data

import arx.engine.entity.Taxon

object Taxonomy {
	val UnknownThing = Taxon("unknown thing", Nil)

	val TerrainType = Taxon("terrain type")
	object Terrain {
		val WetTerrain = Taxon("wet terrain", TerrainType)
	}

	val SourceType = Taxon("source", Nil)

	object Source {
		val Animal = Taxon("animal", SourceType)
		val Plant = Taxon("plant", SourceType)
		val Mineral = Taxon("mineral", SourceType)

		val Fish = Taxon("mackerel", Animal)
	}
}