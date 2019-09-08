package arx.tyche.game.components

import arx.core.datastructures.Watcher
import arx.core.units.UnitOfTime
import arx.engine.game.GameEngine
import arx.engine.game.components.GameComponent
import arx.tyche.game.data.{Globe, Patch}
import arx.Prelude._

class TerrainGameComponent(ge : GameEngine) extends GameComponent(ge) {
	val revWatch = Watcher(world[Globe].revision.get())

	override protected def updateSelf(dt: UnitOfTime): Unit = {

		var modified = false

		for (patchEnt <- world.entitiesWithData[Patch]) {
			// update hydration
			val patch = patchEnt[Patch]
			if (patch.hydration < Patch.MaxHydration) {
				val targetHydration = patch.patchesWithinRange(1).map(p => p[Patch].hydration).max - 1
				modified ||= patch.hydration != targetHydration
				patch.hydration = targetHydration
			}
		}

		if (modified) {
			world[Globe].markModified()
		}
	}

	override protected def needsUpdate: Boolean = timeSinceLastUpdate > 0.75.second && revWatch.hasChanged
}
