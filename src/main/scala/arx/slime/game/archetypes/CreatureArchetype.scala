package arx.slime.game.archetypes

/**
  * TODO: Add javadoc
  */

import arx.core.traits.TSentinel
import arx.engine.entity.GameArchetype
import arx.engine.entity.GameEntity
import arx.engine.entity.TArchetypeKind
import arx.engine.entity.TGameEntity
import arx.slime.game.data.CreatureData
import arx.slime.game.data.PhysicalData

abstract class CreatureArchetype(nomen : String) extends GameArchetype(nomen, CreatureArchetype) {
	val baseCreatureData = this.apply[CreatureData]
	var baseChromosomeCount = 5

	var counter = 0
	def createCreature(parents : List[TGameEntity]) : TGameEntity = {
		val ret = new GameEntity(nomen + " " + counter)
		counter += 1

		ret.copyDataFrom[CreatureData](this)
		ret[PhysicalData]

		ret.archetype = this

		ret
	}
}

object CreatureArchetype extends TArchetypeKind {
	val Sentinel = new CreatureArchetype("Sentinel") with TSentinel {
		override def createCreature(parents: List[TGameEntity]): TGameEntity = GameEntity.Sentinel
	}
}