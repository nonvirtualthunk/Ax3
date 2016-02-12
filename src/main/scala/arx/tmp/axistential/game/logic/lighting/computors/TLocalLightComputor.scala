package arx.axistential.game.logic.lighting.computors

import arx.core.vec.Vec3i
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.ExtendedGenericTaleaGridWindow
import arx.tmp.game.logic.datastructures.ITalea
import arx.tmp.game.logic.entities.TLightSource
import arx.tmp.game.logic.lighting.LightingComponent
import arx.tmp.game.logic.world.data.LightData
import nowiamthefastest.game.logic.components.computors.TLightComputor

/**
 *
 */

trait TLocalLightComputor extends TLightComputor {
	def canHandleLightSource (ls:TLightSource) : Boolean
	def addLightSource ( env : World , lightSource : TLightSource )
	def moveLightSource ( env : World , lightSource : TLightSource, oldWorldPos : Vec3i, newWorldPos : Vec3i )

	/**
	 * Called after a light source has been removed from a local light channel. This method is responsible
	 * for updating hte light grid to reflect this fact. At the time this call is made, the local light
	 * channel in question will no longer contain <code>lightSource</code> on its list of light sources.
 *
	 * @param env the environment formerly containing the light source
	 * @param lightSource the light source that has just been removed
	 * @param fromPos the position the light occupied prior to its removal
	 */
	def removeLightSource ( env : World , lightSource : TLightSource , fromPos : VoxelCoord )

	def updateLightingForTalea( env : World , lightChannel : LightData.LocalLightChannel , lightTalea: LightData.LightTaleaType) : Set[ITalea[_]]


	def highestAdjacentLightValue ( x : Int ,y : Int, z : Int,
									lightWindow : ExtendedGenericTaleaGridWindow[Byte,LightData.LightTaleaType] ,
									terrainWindow : ExtendedGenericTaleaGridWindow[Byte,_ <: LightingComponent.TerrainTaleaType],
									fullLight : Byte ) : Byte =
	{
		var maxValue = 0
		var counter = 0
		while ( counter < 18 ) {
			val curValue = lightWindow(x + Cardinals.expandedCardinals(counter).x,y + Cardinals.expandedCardinals(counter).y,z + Cardinals.expandedCardinals(counter).z)
			maxValue = scala.math.max( maxValue , if ( counter < 6 ) { curValue - 2 } else { curValue - 3 } )
		counter += 1}

		val transp = transparency( terrainWindow(x,y,z) )
		if ( transp <= 0 ) {
			(maxValue + transp).toByte
		} else {
			maxValue.toByte
		}
	}
}