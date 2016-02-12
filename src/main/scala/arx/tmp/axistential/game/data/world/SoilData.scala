package arx.axistential.game.data.world

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/14/13
 * Time: 11:24 AM
 */

import arx.Prelude._
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.data.TWorldAuxData
import arx.tmp.game.logic.datastructures.ByteTalea
import arx.tmp.game.logic.datastructures.CellGrid2D
import arx.tmp.game.logic.datastructures.GenericTalea
import arx.tmp.game.logic.datastructures.GenericTaleaGrid

@SerialVersionUID(1L)
class SoilData extends TWorldAuxData with Serializable {
	val rainfallUnitOfMeasure = 1.cm
	val rainfall = CellGrid2D[Byte](1) //measured in centimeters of rainfall per year

	/** Base Level of Soil Moisture, from rainfall, etc */
	val baseSoilMoisture = 60.0f
	/** Variation from the base soil moisture level, from water sources, etc, will tend toward 0 */
	val deltaSoilMoisture = new GenericTaleaGrid[Byte,ByteTalea](0.toByte,(v:VoxelCoord) => new ByteTalea (v,0.toByte ))
	def moistureLevel ( v : VoxelCoord ) = baseSoilMoisture + deltaSoilMoisture(v)
	def moistureLevel ( x : Int , y : Int , z : Int ) = baseSoilMoisture + deltaSoilMoisture(x,y,z)


	var baseSoilNutrients = 40.0f
	val deltaSoilNutrients = new GenericTaleaGrid[Short,GenericTalea[Short]](0.toShort,(v:VoxelCoord) => new GenericTalea[Short] (v,0.toShort ))

	def nutrientLevel ( v : VoxelCoord ) = baseSoilNutrients + deltaSoilNutrients(v)
	def nutrientLevel ( x : Int , y : Int , z : Int ) = baseSoilNutrients + deltaSoilNutrients(x,y,z)


	val shadeGrid = CellGrid2D[Byte](1)
}