package arx.eldr.game.logic.light

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.datastructures.voxel.Talea
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.units.UnitOfTime
import arx.core.vec.Cardinals
import arx.eldr.game.world.data.Terrain
import arx.engine.game.GameEngine
import arx.engine.game.components.GameComponent
import scalaxy.loops._

class LightComponent(engine:GameEngine) extends GameComponent(engine) {
	val globalComputor = new StandardGlobalLightComputor(world)

	override protected def initialize(): Unit = {
		for ((primaryQ,gli) <- Map(Cardinals.Bottom -> 0)) {
			globalComputor.preInitializeLighting(primaryQ,gli)
			val fullRegion = VoxelRegion(world[Terrain].material.allTaleae.map(_.position))
			val max = fullRegion.max.plusXYZ(Talea.dimension,Talea.dimension,Talea.dimension*2)
			val min = fullRegion.min.plusXYZ(-Talea.dimension,-Talea.dimension,-Talea.dimension*2)

			for (z <- max.z to min.z by -Talea.dimension;
				y <- min.y to max.y by Talea.dimension;
				x <- min.x to max.x by Talea.dimension) {
				globalComputor.preInitializeLightingForTalea(Talea.toTaleaPos(x,y,z),primaryQ,gli)
			}

			for (z <- max.z to min.z by -Talea.dimension;
				  y <- min.y to max.y by Talea.dimension;
				  x <- min.x to max.x by Talea.dimension) {
				globalComputor.initializeLightingForTalea(Talea.toTaleaPos(x,y,z),primaryQ,gli)
			}
		}
	}

	override protected def update(dt: UnitOfTime): Unit = {

	}
}
