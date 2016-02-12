package arx.axistential.testbed.thermo.logic

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/21/13
 * Time: 9:15 AM
 */

import arx.core.representation.ConfigValue
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.data.TWorldAuxData
import arx.tmp.game.logic.datastructures.GenericTalea
import arx.tmp.game.logic.datastructures.GenericTaleaGrid
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.data.TConfigurableGameEntityAuxData
import arx.tmp.game.logic.entities.data.TInheritableAuxData

import scala.collection.mutable


@SerialVersionUID(1L)
class ThermodynamicData extends TWorldAuxData {
	var heatGridsBySource = new mutable.HashMap[GameEntity,ThermodynamicData.ThermalChannel]
	var heatGrids = new SpaceConservingArray[ThermodynamicData.ThermalChannel]()

	var temperatureDelta = new GenericTaleaGrid[Short,GenericTalea[Short]](0,(v:VoxelCoord) => new GenericTalea[Short] (v,0 ))
	var baseTemperature = 0

	def temperatureAt ( x : Int , y : Int , z : Int ) = temperatureDelta(x,y,z) + baseTemperature

	def heatGridsDefinedAt ( v : VoxelCoord ) = {
		heatGrids.denseArraySnapshot.filter( ch => ch.grid.definedAt(v) ).map(_.grid.taleaFor(v.x,v.y,v.z)).toArray
	}
}
object ThermodynamicData {
	case class ThermalChannel ( grid : GenericTaleaGrid[Short,GenericTalea[Short]] , source : GameEntity )
}

trait THeatSource extends GameEntity {
	var heatStrength = 100
	def position : VoxelCoord
}

class HeatSourceData extends TConfigurableGameEntityAuxData with TInheritableAuxData {
	var heatStrength = 100

	def createFromSML(sml: ConfigValue): Option[HeatSourceData] = {
		if ( sml.hasField("heatStrength") ) {
			val hsd = new HeatSourceData
			hsd.heatStrength = sml.heatStrength.int
			Some(hsd)
		} else {
			None
		}
	}


}