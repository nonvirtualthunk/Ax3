package arx.rog2.engine

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.engine.data.TGameEntityAuxData
import arx.engine.data.WrappedWithData
import arx.engine.entity.TGameEntity
import arx.engine.traits.EngineComponent
import arx.engine.world.World
import arx.rog2.game.data.world.RogData
import scalaxy.loops._

trait RogComponent extends EngineComponent[World] {
	lazy val player = world[RogData].player

	def entitiesWithAuxData[T <: TGameEntityAuxData : Manifest] =
		world.auxDataQuery[T].results.toStream.map(e => new WrappedWithData[TGameEntityAuxData, T, TGameEntity](e))
	def foreachEntityWithAuxData[T <: TGameEntityAuxData : Manifest](f : (WrappedWithData[TGameEntityAuxData, T, TGameEntity]) => Unit): Unit = {
		for (ent <- world.auxDataQuery[T].results) {
			f(new WrappedWithData[TGameEntityAuxData, T, TGameEntity](ent))
		}
	}
}
