package arx.axistential.game.data.world

import arx.axistential.game.archetypes.Material
import arx.core.THasSortKey
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.data.TWorldAuxData
import arx.tmp.game.logic.datastructures.ByteTalea
import arx.tmp.game.logic.datastructures.GenericRawTaleaGrid
import arx.tmp.game.logic.datastructures.GenericTalea
import arx.tmp.game.logic.entities.core.GameEntity

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/9/13
 * Time: 11:31 AM
 * To change this template use File | Settings | File Templates.
 */


class FluidData extends TWorldAuxData with Serializable {
	val fluidLevel = new GenericRawTaleaGrid[Short,GenericTalea[Short]](0,(v:VoxelCoord) => new GenericTalea[Short] (v,0 ))
	val fluidType = new GenericRawTaleaGrid[Byte,ByteTalea](0,(v:VoxelCoord) => new ByteTalea (v,0 ))
	val direction = new GenericRawTaleaGrid[Byte,ByteTalea](0,(v:VoxelCoord) => new ByteTalea (v,0 ))

	val maxFluidLevel = (Short.MaxValue / 2).toShort
	def minFluidCutoff = 800

	@inline
	def definedAt ( v : VoxelCoord ) = fluidLevel.definedAt(v)
	@inline
	def definedAt ( x : Int, y : Int, z : Int ) = fluidLevel.definedAt(x,y,z)
	@inline
	def isFluidAt ( x : Int, y : Int, z : Int ) = (fluidLevel(x,y,z) & 0x7fff)> minFluidCutoff
	@inline
	def isFluidAt ( v : VoxelCoord ) = (fluidLevel(v) & 0x7fff) > minFluidCutoff

	@inline
	def setInfiniteFluidLevel ( x : Int , y : Int ,z : Int , level : Short, typeByte : Byte ) {
		fluidLevel.setUnsafe(x,y,z,(level | ~0x7fff).toShort)
		fluidType.setUnsafe(x,y,z,typeByte)
	}

	@inline
	def effectiveFluidLevel ( x : Int , y : Int , z : Int ) = fluidLevel(x,y,z) & 0x7fff

	var infiniteFluidSources : List[InfiniteFluidSource] = Nil
}

class InfiniteFluidSource (val location : VoxelCoord, val fluidType : Material ) extends GameEntity {
	var hasOutlet = false
}

case class InfiniteFluidNode(x:Int,y:Int,z:Int,g:Float,from:Int) extends THasSortKey{
	def sortKey: Float = (z - VoxelCoord.Center.z).toFloat + g * 0.05f
}