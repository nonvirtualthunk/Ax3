package arx.rog2.game.data.world

/**
  * TODO: Add javadoc
  */

import arx.core.datastructures.FiniteGrid2D
import arx.core.datastructures.ShortMapping
import arx.core.datastructures.voxel.VoxelGrid
import arx.core.traits.ArxEnum
import arx.core.traits.ArxEnumObject
import arx.core.vec.ReadVec2i
import arx.core.vec.Vec2i
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.data.TWorldAuxData
import arx.rog2.game.data.world.Terrain.Modification
import arx.rog2.game.entities.Material

class Terrain extends TWorldAuxData {
	protected[data] val materialGrid = new VoxelGrid[Short](0)
	protected[data] val materialMapping = new ShortMapping[Material](Material.Sentinel)
	protected[data] val highestSolid = FiniteGrid2D.apply[Short](VoxelCoord.Center - 512, Vec2i(1024), 0.toShort)
	protected[data] val flags = new VoxelGrid[Short](0.toShort)
	var modificationCount = 0
	var modifications = List[Modification]()
	var recordModifications = false

	def voxel(x: Int, y: Int, z: Int) = MaterialVoxelRef(this, VoxelCoord(x, y, z))

	def voxel(v: VoxelCoord) = MaterialVoxelRef(this, v)

	def setMaterial(v: VoxelCoord, mat: Material): Unit = {
		val cur = materialMapping(materialGrid(v))
		materialGrid(v) = materialMapping(mat)
		if (mat.solid) {
			val cur = highestSolid(v.xy).toInt
			val next = cur.max(v.z).toShort
			highestSolid(v.xy) = next
		}
		if (recordModifications) {
			modifications ::= Modification(v.x, v.y, v.z, cur, mat, modificationCount)
			modificationCount += 1
		}
	}

	def highestSolidZ(v: VoxelCoord) = highestSolid(v.xy)

	def highestSolidZ(v2: ReadVec2i) = highestSolid(v2)

	def highestSolidV(v: VoxelCoord) = VoxelCoord(v.x, v.y, highestSolid(v.xy))

	def highestSolidV(v2: ReadVec2i) = VoxelCoord(v2.x, v2.y, highestSolid(v2))
}

object Terrain {

	case class Modification(x: Int, y: Int, z: Int, from: Material, to: Material, idx: Int)

}

case class MaterialVoxelRef(terrain: Terrain, v: VoxelCoord) {
	def set(m: Material): this.type = {
		terrain.setMaterial(v, m)
		this
	}
	def material = terrain.materialMapping(terrain.materialGrid(v))

	def hasFlag(flag : TerrainFlag) = (terrain.flags(v) & flag.bitmask) == flag.bitmask
	def setFlag(flag : TerrainFlag) : this.type = {
		terrain.flags(v) = (terrain.flags(v) | flag.bitmask).toShort
		this
	}
	def unsetFlag(flag : TerrainFlag) = {
		terrain.flags(v) & (~flag.bitmask)
		this
	}
}

object MaterialVoxelRef {
	implicit def toMaterial(mvr: MaterialVoxelRef): Material = mvr.material
}

case class TerrainFlag(nomen:String, ordinal: Int) extends ArxEnum(nomen) {
	val bitmask = 1 << ordinal
}
object TerrainFlag extends ArxEnumObject[TerrainFlag] {
	val Ceiling = TerrainFlag("Ceiling", 0)
	val Floor = TerrainFlag("Floor", 1)
	val Wall = TerrainFlag("Wall", 2)
}