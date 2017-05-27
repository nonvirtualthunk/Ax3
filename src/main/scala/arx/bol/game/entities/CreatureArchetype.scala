package arx.bol.game.entities

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.bol.game.entities.data.Behavior
import arx.bol.game.entities.data.CreatureData
import arx.bol.game.entities.data.PhysicalData
import arx.core.vec.Vec2f
import arx.core.vec.Vec2i
import arx.engine.entity.GameArchetype
import arx.engine.entity.GameEntity
import arx.engine.entity.TArchetypeKind

import scalaxy.loops._

class CreatureArchetype(nomen : String) extends GameArchetype(nomen, CreatureArchetype) {

	override def create() = {
		val entity = new GameEntity(nomen)

		entity.copyDataFrom[CreatureData](this)
		entity.copyDataFrom[PhysicalData](this)
		entity.archetype = this

		entity
	}
}

object CreatureArchetype extends TArchetypeKind {
	val Sentinel = new CreatureArchetype("Sentinel")

	val Human = new CreatureArchetype("human")
	Human.aux[PhysicalData].size = Vec2i(2,4)
	val Skeleton = new CreatureArchetype("skeleton")
	Skeleton.aux[PhysicalData].size = Vec2i(2,4)
}