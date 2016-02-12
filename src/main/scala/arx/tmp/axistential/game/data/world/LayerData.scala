package arx.axistential.game.data.world

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/6/13
 * Time: 1:32 PM
 */

import arx.Prelude._
import arx.axistential.game.archetypes.Material
import arx.axistential.game.data.world.GeologicalLayer.LayerType
import arx.core.representation.ConfigValue
import arx.core.representation.Hocon
import arx.core.representation.TConfigurable
import arx.core.traits.TArxEnum
import arx.engine.data.TWorldAuxData
import arx.tmp.game.logic.datastructures.CellGrid2D
import arx.tmp.game.logic.entities.core.GameArchetype

import scalaxy.loops._

class LayerData extends TWorldAuxData {
	var minZ = 0
	var maxZ = 0
	var ZRange = 0

	def layers_= ( l : List[GeologicalLayer] ) {
		posit(world.notSentinel,"Geological Layer Data has a sentinel world, and layers are being set")
		_layers = l
		minZ = world.worldRegionAsSpatialRegion.minZ
		maxZ = world.worldRegionAsSpatialRegion.maxZ
		ZRange = maxZ - minZ

		val matMap = world.aux[TerrainData].materialMapping
		for ( layer <- layers ) {
			layer.mainMaterialBytes = layer.mainMaterials.map( mat => matMap(mat) ).toArray
		}

		var layerIndex = 0
		layersByZ = Array.ofDim(ZRange)
		for ( z <- minZ until maxZ optimized ) {
			layersByZ(z - minZ) = layers(layerIndex)

		}
	}
	def layers = _layers

	protected var _layers = List[GeologicalLayer]()
	var layersByZ = Array[GeologicalLayer]()
	val layerShifts = CellGrid2D[Byte](8)

	def layerShiftView ( x : Int, y : Int ) = layerShifts.cellView(x,y)
	def layer ( x : Int, y : Int, z : Int , layerShift : Int ) = {
		layersByZ( math.max(0,math.min(ZRange-1,z - minZ + layerShift)) )
	}

	layers = new ConfiguredGeologicalLayer(Hocon.parse("{ mainMaterials : [ stone ] , layerType : Metamorphic }")) :: Nil
}


class GeologicalLayer extends GameArchetype {
	var mainMaterials : List[Material] = Nil
	var mainMaterialBytes : Array[Byte] = Array.ofDim(0)
	var layerType : LayerType = GeologicalLayer.Type.Metamorphic
}
object GeologicalLayer {
	class LayerType( name : String ) extends TArxEnum { def key = name }
	object LayerType { def apply ( str : String ) = TArxEnum.existing[LayerType](str) match {
			case Some(existing) => existing
			case _ => new LayerType(str)
	}}

	object Type {
		val Metamorphic = new LayerType("Metamorphic")
		val Sedimentary = new LayerType("Sedimentary")
		val Igneous = new LayerType("Igneous")
		val Soil = new LayerType("Soil")
		val Clay = new LayerType("Clay")
	}
}

class ConfiguredGeologicalLayer( initialSML : ConfigValue ) extends GeologicalLayer with TConfigurable {
	var sml = initialSML
	setFromSML(sml)


	def setFromSML(sml: ConfigValue, overwrite: Boolean): Unit = {
		mainMaterials = sml.mainMaterials.arr.map( v => Material.materialWithName(v.str) ).toList
		if ( sml.hasField("layerType") ) {
			layerType = GeologicalLayer.LayerType( sml.layerType.str )
		}
	}
}
