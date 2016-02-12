package arx.axistential.graphics.components.renderers

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/19/12
 * Time: 2:35 PM
 * Created by nonvirtualthunk
 */

import arx.axistential.graphics.components.general.AnimationSubVoxel
import arx.engine.world.World
import arx.graphics.TextureBlock
import arx.graphics.traits.TRenderTarget

import scala.collection.GenTraversable

trait TSubVoxelEnvironmentRenderer {
	def renderSubVoxels ( renderTarget : TRenderTarget , textureBlock : TextureBlock , env : World , subVoxel : GenTraversable[AnimationSubVoxel] )
}