package arx.axistential.game.data.world

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/6/13
 * Time: 2:52 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.axistential.game.archetypes.Covering
import arx.axistential.game.archetypes.Material
import arx.axistential.game.constants.StateOfMatter
import arx.axistential.game.data.world.TerrainData.ProxyMaterial
import arx.axistential.game.data.world.networking.TerrainDataDeltaHandler
import arx.core.datastructures.ByteMapping
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.MutableVoxelCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.coordinates.VoxelSideCoord
import arx.engine.data.TWorldAuxData
import arx.tmp.game.logic.datastructures._
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.world.data.TNetworkedWorldData
import arx.tmp.game.networking.NetworkedWorldDataDeltaHandler

import scalaxy.loops._

@SerialVersionUID(1L)
class TerrainData extends TWorldAuxData with TNetworkedWorldData {
	val _materialGrid = new GenericTaleaGrid[Byte,ByteTalea](0,(v:VoxelCoord) => new ByteTalea(v,0)) with TGridWithChangeQueue[Byte,ByteTalea]
	val _materialMapping = new ByteMapping[Material](Material.Sentinel)
	// Initialize the proxy materials that we use for things that don't map directly to materials
	// and are instead defined by their properties (opaque/transparent, liquid-passable, etc)
	for (i <- -1 to -128 by -1 optimized) {
		_materialMapping.set(i.toByte,new ProxyMaterial(i.toByte))
	}

	val _coveringGrid = new FaceTaleaGrid with TGridWithChangeQueue[Int,FaceTalea]
	var _coveringTypes = new ByteMapping[Covering](Covering.Sentinel)

	def materialGrid = _materialGrid
	def materialMapping = _materialMapping
	def coveringTypes = _coveringTypes
	def coveringGrid = _coveringGrid

	@inline def materialForByte ( b : Byte ) = _materialMapping( b )
	@inline def isOccupied ( v : VoxelCoord ) : Boolean = isOccupied(v.x,v.y,v.z)
	@inline def isSolid ( v : VoxelCoord ) : Boolean = materialAt(v).solid
	@inline def isSolid ( x : Int , y : Int, z : Int ) : Boolean = materialAt(x,y,z).solid
	@inline def isOccupied ( x : Int , y : Int, z : Int ) : Boolean = { _materialGrid(x,y,z) != 0 }

	@inline def materialByteAt ( v : VoxelCoord ) : Byte = _materialGrid(v)
	@inline def materialByteAt ( x : Int, y : Int, z : Int ) : Byte = _materialGrid(x,y,z)
	def materialAt ( v : VoxelCoord ) : Material = materialAt(v.x,v.y,v.z)
	def materialAt ( x : Int , y : Int, z : Int ) : Material = {
		_materialMapping( _materialGrid(x,y,z) )
	}

	def setMaterialAt ( v : VoxelCoord , m : Material ) { setMaterialAt(v.x,v.y,v.z,m) }
	def setMaterialAt ( x : Int , y : Int , z : Int , m : Material ) {
		_materialGrid(x,y,z) = _materialMapping(m)
	}

	def isEntityAt ( v : VoxelCoord ) = _materialGrid(v) < 0
	def setEntityAt ( entity : GameEntity , v : VoxelCoord , flags : Int ) {
		_materialGrid(v) = (~flags).toByte
	}
	def setEntityAt (  x : Int , y : Int , z : Int , entity : GameEntity , flags : Int ) {
		_materialGrid(x,y,z) = (~flags).toByte
	}
	def removeEntityAt ( entity : GameEntity , v : VoxelCoord ) {
		if ( _materialGrid(v) < 0 ) {
			_materialGrid(v) = 0.toByte
		}
	}

	def coveringAt(coord: VoxelSideCoord) = {
		coveringTypes(coveringGrid.apply(coord.voxel.x,coord.voxel.y,coord.voxel.z,coord.side))
	}
	def coveringAt(coord: VoxelCoord, side : Int) = {
		coveringTypes(coveringGrid.apply(coord.x,coord.y,coord.z,side))
	}
	def setCoveringAt(coord: VoxelSideCoord, newCovering : Covering): Unit = {
		val newCoveringByte = coveringTypes(newCovering)
		coveringGrid(coord) = newCoveringByte
	}


	def _setFromFunction ( func : (Int,Int,Int) => Material , range : Int ) {
		val min = VoxelCoord.Center + Vec3i(-range)
		val max = VoxelCoord.Center + Vec3i(range)
		for ( x <- min.x to max.x ; y <- min.y to max.y ; z <- min.z to max.z ) {
			setMaterialAt(x,y,z,func(x,y,z))
		}
	}

	def firstNonSolidInColumn ( v : VoxelCoord , maximumZDelta : Int = 128) : VoxelCoord = {
		val ret = MutableVoxelCoord(v)
		ret.z = VoxelCoord.Center.z + maximumZDelta
		while ( ret.z >= VoxelCoord.Center.z - maximumZDelta ) {
			if ( ! materialAt(ret).solid ) { return ret }
			ret.z -= 1
		}
		VoxelCoord.Sentinel
	}

	def firstSolidInColumn ( v : VoxelCoord , maximumZDelta : Int = 128) : VoxelCoord = {
		val ret = MutableVoxelCoord(v)
		ret.z = VoxelCoord.Center.z + maximumZDelta
		while ( ret.z >= VoxelCoord.Center.z - maximumZDelta ) {
			if ( materialAt(ret).solid ) { return ret }
			ret.z -= 1
		}
		VoxelCoord.Sentinel
	}

	override def createDeltaHandler: NetworkedWorldDataDeltaHandler[_] = new TerrainDataDeltaHandler(this)
}

object TerrainData {
	protected class ProxyMaterial(flags : Byte) extends Material with NonDiscoverable {
		name = "Entity Placeholder " + flags
		solid = TerrainByteUtils.isSolid(flags)
		density = kg_m3
		opacity = if (TerrainByteUtils.isOpaque(flags)) { 1.0f } else if ( TerrainByteUtils.isPartiallyTransparent(flags) ) { 0.5f } else { 0.0f }
		_byteTransparency = TerrainByteUtils.lightComputorTransparency(flags)
		stateOfMatter = StateOfMatter.Solid
		strength = 10.0f
		insulation = 0.0f


		horizontalSupportStrength = 0.kg
		verticalSupportStrength = 0.kg

		override val conditionallyPassable: Boolean = TerrainByteUtils.isConditionallyPassable(flags)
	}
}
