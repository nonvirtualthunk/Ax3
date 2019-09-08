package arx.tyche.game.components

import arx.core.units.UnitOfTime
import arx.engine.game.GameEngine
import arx.engine.game.components.GameComponent
import arx.tyche.game.data.{CreateSource, Globe, Patch, Spirit, TerrainType, TransformTerrain}
import arx.Prelude._
import arx.core.math.Interpolation

class SpiritGameComponent(ge : GameEngine) extends GameComponent(ge) {

	override protected def updateSelf(dt: UnitOfTime): Unit = {
		world.entitiesWithData[Spirit].foreach(spiritEnt => {
			val spirit = spiritEnt[Spirit]
			val pos = spirit.position
			for  (dest <- spirit.destination) {
				spirit.position = pos.moveTowards(dest, spirit.speed)
			}

			for (action <- spirit.intendedAction) {
				if (spirit.canPerformAction(world, action)) {
					action.progress += dt

					if (action.progress >= action.actionType.timeToAct) {
						action.actionType match {
							case TransformTerrain(targetTerrain, patchShape) =>
								var delay = 0.seconds
								for (patch <- patchShape.patchesIfAt(world, action.patch)) {
									world[Transitions].transitions ::= Transition[TerrainType](
										Interpolation.constant(targetTerrain),
										(t: TerrainType) => {
											patch[Patch].terrainType = t
											if (t == TerrainType.Ocean) {
												patch[Patch].hydration = Patch.MaxHydration
											}
											world[Globe].markModified()
										},
										delay,
										0.seconds
									)
									delay += 1.seconds
								}
							case CreateSource(_, sources) => {
								for (src <- sources.get(action.patch[Patch].terrainType)) {
									action.patch[Patch].sourceType = src
								}
							}
						}
						spirit.intendedAction = None
					}
				}
			}
		})
		dt.inSeconds
	}
}
