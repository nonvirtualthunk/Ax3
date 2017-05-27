package arx.rog2.game.engine

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.units.UnitOfTime

import scalaxy.loops._
import arx.rog2.game.data.world._
import arx.rog2.game.data.entity._
import arx.engine.entity.TGameEntity
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec._
import arx.engine.game.GameEngine
import arx.engine.game.components.GameComponent
import arx.rog2.engine.RogComponent

class RogCreatureGameComponent(eng : GameEngine) extends GameComponent(eng) with RogComponent {
	override protected def update(dt: UnitOfTime): Unit = {
		for (ent <- entitiesWithAuxData[Creature]) {
			ent.hunger += dt.inSeconds / 100.0f
		}
	}
}
