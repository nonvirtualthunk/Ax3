package arx.rog2.game.engine

/**
  * TODO: Add javadoc
  */

import arx.ai.search.{PathQuery, Searcher}
import arx.application.Noto
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.game.GameEngine
import arx.engine.game.components.GameComponent
import arx.rog2.control.AdvanceWorldEvent
import arx.rog2.engine.RogComponent
import arx.rog2.game.actions.{Action, AttackAction, DoNothingAction, MoveAction}
import arx.rog2.game.data.entity._
import arx.rog2.game.data.world._
import arx.rog2.game.events.EntityMovedEvent
import arx.Prelude._

class RogCreatureGameComponent(eng : GameEngine, vision : RogVisionGameComponent) extends GameComponent(eng) with RogComponent {


	override protected def updateSelf(dt: UnitOfTime): Unit = {
		for (ent <- entitiesWithAuxData[Creature]) {
			ent.hunger += dt.inSeconds * ent.metabolism

			if (ent != player) {
				val playerPos = player[Physical].position

				val action : Action = if (vision.isVisibleTo(looker = ent, target = player)) {
						if (ent[Physical].position.distanceTo(player[Physical].position) < 1.75.voxels) {
							AttackAction(ent, player)
						} else {
							Searcher.pathTo(new PathQuery(world, ent[Physical].position, VoxelRegion.hollowCubeFromCorners(playerPos - 1,playerPos + 1)) {
								val terrain = world[Terrain]
								obstructionFunction = {
									v => terrain.voxel(v).material.solid
								}
								isSupportedFunction = {
									v => terrain.voxel(v.minusZ(1)).material.solid
								}
							}) match {
								case Some(path) if path.size > 1 =>
									MoveAction(ent, ent[Physical].position, path(1))
								case None =>
									Noto.info("No path found")
									DoNothingAction(ent)
							}
						}
					} else {
						DoNothingAction(ent)
					}

				executeAction(action)
			}
		}
	}


	def executeAction(action: Action): Unit = {
		if (action.isValid(world)) {
			val events = action.apply(world)
			events.foreach(eventBus.fireEvent)
			if (action.actor == player) {
				eventBus.fireEvent(AdvanceWorldEvent(action.timeRequired(world)))
			}
		}
	}
}
