package arx.eldr.game.archetypes

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.core.traits.TSentinel
import arx.engine.entity.GameArchetype
import arx.engine.entity.TArchetypeKind
import scalaxy.loops._

class Skill(nomen : String, val profession : Profession) extends GameArchetype(nomen, Skill) {

}

object Skill extends TArchetypeKind {
	def apply (nomen : String, prof : Profession) = {
		val result = new Skill(nomen,prof)
		result
	}
	val PlantGathering = Skill("Plant Gathering", Profession.Farming)
	val WoodCutting = Skill("Wood Cutting", Profession.Farming)

	val Sentinel : Skill = new Skill("Sentinel",Profession.Sentinel) with TSentinel
}


class Profession(nomen : String) extends GameArchetype(nomen, Profession) {

}

object Profession extends TArchetypeKind {
	def apply (nomen : String) = {
		val result = new Profession(nomen)
		result
	}

	val Farming = Profession("Farming")

	val Sentinel : Profession = new Profession("Sentinel") with TSentinel
}