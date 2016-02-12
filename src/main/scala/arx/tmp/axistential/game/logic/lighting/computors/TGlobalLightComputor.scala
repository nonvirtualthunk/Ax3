package arx.axistential.game.logic.lighting.computors

import arx.core.vec.ReadVec3i
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.ExtendedGenericTaleaGridWindow
import arx.tmp.game.logic.datastructures.ITalea
import arx.tmp.game.logic.datastructures.TTaleaGrid
import arx.tmp.game.logic.datastructures.Talea
import arx.tmp.game.logic.lighting.LightingComponent
import arx.tmp.game.logic.world.data.LightData
import nowiamthefastest.game.logic.components.computors.TLightComputor


/**
 *
 */

trait TGlobalLightComputor extends TLightComputor {
	val minimumZ = -Talea.dimension * 2
	val absoluteMinimumZ = -Talea.dimension * 5
	val absoluteMaximumZ = Talea.dimension * 5

	def preInitializeLighting (env : World, primaryQ : Int, gli : Int){}
	def preInitializeLightingForTalea ( env : World , loc : ReadVec3i, primaryQ : Int, gli : Int )
	def initializeLightingForTalea ( env : World , loc : ReadVec3i, primaryQ : Int, gli : Int )

	def updateLightingForTalea( env : World ,
										 lightGrid : TTaleaGrid[Byte,LightData.LightTaleaType] ,
										 lightTalea : LightData.LightTaleaType,
										 primaryQ : Int, gli : Int,
										 localLighting : Boolean = false ) : Set[ITalea[_]]


	def highestAdjacentLightValue ( x : Int ,y : Int, z : Int,
									lightWindow : ExtendedGenericTaleaGridWindow[Byte,LightData.LightTaleaType] ,
									terrainWindow : ExtendedGenericTaleaGridWindow[Byte,_ <: LightingComponent.TerrainTaleaType],
									fullLight : Byte ) : Byte =
	{
		var maxValue = 0
		var counter = 0
		while ( counter < 6 ) {
			val curValue = lightWindow(x + cardinals(counter).x,y + cardinals(counter).y,z + cardinals(counter).z)
			maxValue = scala.math.max( maxValue , (if ( curValue == fullLight && counter == Cardinals.Top ) { curValue } else { curValue - 1 }) )
		counter += 1}

		val transp = transparency( terrainWindow(x,y,z) )
		if ( transp <= 0 ) {
			(maxValue + transp).toByte
		} else {
			maxValue.toByte
		}
	}
}