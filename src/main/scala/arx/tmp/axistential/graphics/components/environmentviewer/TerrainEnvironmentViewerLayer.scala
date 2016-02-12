package arx.axistential.graphics.components.environmentviewer

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/20/13
 * Time: 12:55 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.axistential.game.data.world.TerrainData
import arx.axistential.graphics.components.renderers.THighDetailEnvironmentRenderer
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.world.World
import arx.tmp.game.logic.world.data.LightData

class TerrainEnvironmentViewerLayer extends TEnvironmentViewerLayer {
	def revisionOf ( env : World , loc : VoxelCoord ) = {
		val lightData = env.aux[LightData]
		val terrain = env.aux[TerrainData]

		var currentRevision = 0L

		val lightRevision = lightData.lightRevisionAt(loc)
		val terrainModifications = terrain._materialGrid.getModifiedCountIncludingAdjacents(loc)
		val coveringModifications = terrain._coveringGrid.getModifiedCountIncludingAdjacents(loc)

		var i = 0
		i = 0;while ( i < terrainModifications.length ) { currentRevision += terrainModifications(i) ; i += 1 }
		i = 0;while ( i < coveringModifications.length ) { currentRevision += coveringModifications(i) ; i += 1 }
		currentRevision += lightRevision

		currentRevision
	}

	def renderer = pio[THighDetailEnvironmentRenderer]
}