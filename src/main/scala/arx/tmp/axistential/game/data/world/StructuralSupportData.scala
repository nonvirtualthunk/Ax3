package arx.axistential.game.data.world

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/23/14
 * Time: 8:05 AM
 */

import arx.core.vec.coordinates.VoxelCoord
import arx.engine.data.TWorldAuxData
import arx.tmp.game.logic.datastructures.GenericTalea
import arx.tmp.game.logic.datastructures.GenericTaleaGrid

import scala.collection.mutable

class StructuralSupportData extends TWorldAuxData {
	val supportGrid = new GenericTaleaGrid[Int,GenericTalea[Int]](StructuralSupportData.PackedUninitialized,(v:VoxelCoord) => new GenericTalea[Int] (v,StructuralSupportData.PackedUninitialized ))
	val XArches = new mutable.HashMap[VoxelCoord,Arch]()
	val YArches = new mutable.HashMap[VoxelCoord,Arch]()
	val archMaps = Array(XArches,YArches)

	def setSupport (v : VoxelCoord, source : Short,support : Short) {
		supportGrid(v) = StructuralSupportData.pack(source,support)
	}
	def setSupport (v : VoxelCoord, source : Int,support : Int) {
		supportGrid(v) = StructuralSupportData.pack(source.toShort,support.toShort)
	}
	def setSource (v : VoxelCoord, source : Int) {
		val curSupport = support(v)
		supportGrid(v) = StructuralSupportData.pack(source.toShort,curSupport)

	}
	import arx.axistential.game.data.world.StructuralSupportData._
	def support (v : VoxelCoord) = supportGrid(v).support
	def source (v : VoxelCoord) : Short = supportGrid(v).source

	def support (x:Int,y:Int,z:Int) = supportGrid(x,y,z).support
	def source (x:Int,y:Int,z:Int) = supportGrid(x,y,z).source
}

@SerialVersionUID(1L)
case class Arch(leftChain : List[VoxelCoord], rightChain : List[VoxelCoord], xyi : Int) extends Serializable {
	def chains = Array(leftChain,rightChain)
	def keystone = leftChain.last
	def allBlocks = leftChain ::: rightChain.dropRight(1)
	def allNonKeystoneBlock = leftChain.dropRight(1) ::: rightChain.dropRight(1)
}

object StructuralSupportData {
	def unpackSource (s : Int) : Short = {
		(s >> 16).toShort
	}
	def unpackSupport (s : Int) : Short = {
		(s & 0xffff).toShort
	}

	def pack (source : Int, support : Int) : Int = {
		pack(source.toShort,support.toShort)
	}
	def pack (source : Short, support : Short) : Int = {
		(support & 0xffff) | (source << 16)
	}

	implicit class StructuralSupportValue(val s:Int) extends AnyVal {
		def source = unpackSource(s)
		def support = unpackSupport(s)
		def isUninitialized = s == PackedUninitialized
		def isRootBlock = (source & RootBlock) != 0
		def nonRootBlock = ! isRootBlock
	}
	
	implicit class SupportDirectionStruct(val s : Short) extends AnyVal {
		def isXVoussoir = (s & XVoussoirBit) != 0
		def isYVoussoir = (s & YVoussoirBit) != 0
		def isVoussoir(xyi:Int) = (s & VoussoirBits(xyi)) != 0
		def isVoussoir = (s & AnyVoussoirMask) != 0
		def isXKeystone = (s & XKeystoneBit) != 0
		def isYKeystone = (s & YKeystoneBit) != 0
		def isKeystone(xyi:Int) = (s & KeystoneBits(xyi)) != 0
		def isKeystone = (s & AnyKeystoneMask) != 0
		def isInArch = (s & AnyArchMask) != 0
		def isInArch(xyi:Int) = (s & AnyArchMasks(xyi)) != 0
		def isInXArch = (s & AnyXArchMask) != 0
		def isInYArch = (s & AnyYArchMask) != 0
		def direction = s & DirectionMask
		def isRootBlock = (s & RootBlock) != 0
		def nonRootBlock = ! isRootBlock
	}

	final val MaxSupportValue : Int = (1 << 15) - 1
	final val MinSupportValue : Int = -(1 << 15)

	final val XVoussoirBit = 1 << 14
	final val YVoussoirBit = 1 << 13
	final val AnyVoussoirMask = XVoussoirBit|YVoussoirBit
	final val XKeystoneBit = 1 << 12
	final val YKeystoneBit = 1 << 11
	final val AnyKeystoneMask = XKeystoneBit|YKeystoneBit
	final val DirtyBit = 1 << 10
	final val DirectionMask = 0x00ff
	final val AnyArchMask = AnyVoussoirMask|AnyKeystoneMask
	final val AnyXArchMask = XVoussoirBit|XKeystoneBit
	final val AnyYArchMask = YVoussoirBit|YKeystoneBit
	final val AnyArchMasks = Array(AnyXArchMask,AnyYArchMask)

	final val VoussoirBits = Array(XVoussoirBit,YVoussoirBit)
	final val KeystoneBits = Array(XKeystoneBit,YKeystoneBit)

	final val Xi = 0
	final val Yi = 1
	
	final val RootBlock = (1 << 15).toShort
	
	final val PackedUninitialized = pack(RootBlock,0)
}