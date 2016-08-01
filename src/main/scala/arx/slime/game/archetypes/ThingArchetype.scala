package arx.slime.game.archetypes

/**
  * TODO: Add javadoc
  */

import arx.engine.entity.GameArchetype
import arx.engine.entity.TArchetypeKind

class ThingArchetype(nomen : String) extends GameArchetype(nomen, ThingArchetype) {

}

object ThingArchetype extends TArchetypeKind {
	val Sentinel = new ThingArchetype("Sentinel")

	val Relic = new ThingArchetype("Precursor Relic")
}