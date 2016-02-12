package arx.axistential.graphics.components.environmentviewer

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/20/13
 * Time: 12:55 PM
 * Created by nonvirtualthunk
 */

import arx.axistential.graphics.components.renderers.TEnvironmentRenderer
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.world.World

trait TEnvironmentViewerLayer {
	var graphicsEngine : GraphicsEngine = null
	def revisionOf ( env : World , loc : VoxelCoord ) : Long
	def renderer : TEnvironmentRenderer
}