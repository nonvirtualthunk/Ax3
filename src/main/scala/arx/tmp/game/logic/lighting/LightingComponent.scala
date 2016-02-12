package arx.tmp.game.logic.lighting

import arx.core.vec.ReadVec3i
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.ITalea
import arx.tmp.game.logic.datastructures.TLoggingTalea
import arx.tmp.game.logic.entities.TLightSource
import arx.tmp.game.logic.world.data.LightData


case class UpdateLightingTask(lightTalea : LightData.LightTaleaType)
case class BulkUpdateLightingTask(lightTaleae : List[ITalea[_]])
case class AddNewLightSourceTask(lightSource : TLightSource)
case class MoveLightSourceTask(lightSource : TLightSource,oldPos : ReadVec3i,newPos : ReadVec3i)
case class RemoveLightSourceTask(lightSource : TLightSource,pos:VoxelCoord)
case class UpdateLightStrengthTask(lightSource : TLightSource, oldStrength : Byte , newStrength : Byte )

object LightingComponent {
	type TerrainTaleaType = ITalea[Byte] with TLoggingTalea[Byte]
}