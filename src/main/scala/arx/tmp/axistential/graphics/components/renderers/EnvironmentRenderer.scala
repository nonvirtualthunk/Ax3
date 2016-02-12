package arx.axistential.graphics.components.renderers

import arx.core.vec.ReadVec3i
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.TInfiniteVoxelStore
import arx.graphics.AVBO
import arx.graphics.AttributeProfile
import arx.graphics.TextureBlock
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
 *
 */

case class VoxelRenderingSettings (exposedFacesOnly:Boolean,opacity : Float,alternateBlockSource : Option[TInfiniteVoxelStore[Byte]] = None,useOnlyAlternateBlockSource :Boolean= false,
												 useIndices:Boolean=true,facesEnabled : Option[IndexedSeq[Boolean]] = None) {
	var constructedOnly : Boolean = false
}

trait TEnvironmentRenderer {
	def updateTalea ( vbo: AVBO , textureBlock : TextureBlock , env : World , wloc: VoxelCoord , offset : ReadVec3i , limit : ReadVec3i )
	def attributeProfile : AttributeProfile
}
trait THighDetailEnvironmentRenderer extends TEnvironmentRenderer{
	def updateTalea ( vbo: AVBO , textureBlock : TextureBlock , env : World , wloc: VoxelCoord , offset : ReadVec3i , limit : ReadVec3i );
	def updateVoxels ( vbo: AVBO , textureBlock : TextureBlock , env : World , locs: IndexedSeq[ReadVec3i] , locOffset: VoxelCoord , settings : VoxelRenderingSettings) {
		throw new NotImplementedException()
	}
}
trait TLowDetailEnvironmentRenderer extends TEnvironmentRenderer {
	def updateTaleaLowDetail ( vbo: AVBO , textureBlock : TextureBlock , env : World , wloc: VoxelCoord , offset : ReadVec3i , limit : ReadVec3i );
}
trait TCutoffEnvironmentRenderer {
	def updateTaleaCutoff ( vbo : AVBO , textureBlock : TextureBlock , env : World , wloc: VoxelCoord , wcutoff : Int )
}
abstract class EnvironmentRenderer extends THighDetailEnvironmentRenderer with TLowDetailEnvironmentRenderer {
	override def updateTaleaLowDetail ( vbo: AVBO , textureBlock : TextureBlock , env : World , wloc: VoxelCoord , offset : ReadVec3i , limit : ReadVec3i ) {
		updateTalea(vbo,textureBlock,env,wloc,offset,limit)
	}
}