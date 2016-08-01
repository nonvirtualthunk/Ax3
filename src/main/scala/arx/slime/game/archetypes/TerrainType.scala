package arx.slime.game.archetypes

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.traits.TSentinel
import arx.engine.entity.GameArchetype
import arx.engine.entity.TArchetypeKind

import scalaxy.loops._

class TerrainType(nomen : String, val isWater : Boolean, val baseMoveCost : Int) extends GameArchetype(nomen, TerrainType) {

}

object TerrainType extends TArchetypeKind {
	val Sentinel : TerrainType = NoTerrain
}

object Water extends TerrainType("water", true, 1)

object Grass extends TerrainType("grass", false, 1)

object Dirt extends TerrainType("dirt", false, 1)

object Stone extends TerrainType("stone", false, 2)

object NoTerrain extends TerrainType("void", false, 10000) with TSentinel