package arx.tmp.game.logic.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/9/12
 * Time: 8:23 AM
 * Created by nonvirtualthunk
 */

import arx.engine.control.event.Event.TEventUser
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.world.SpatialRegion

import scala.collection.mutable


trait TTaleaGrid[@specialized(Byte,Short,Int) T,TaleaType <: ITalea[T]] extends TInfiniteVoxelStore[T] with TSubContainerStore[ITalea[T]] with TEventUser {
	def enableLogging ()
	def disableLogging ()
	def isLoggingEnabled : Boolean
	def changeLogging (enable : Boolean)

	def clear (b : T)

	def createTalea ( pos : VoxelCoord ) : TaleaType
	def taleaFor(x : Int,y: Int,z: Int) : TaleaType
	def taleaForReadOnly(x : Int,y: Int,z: Int) : TaleaType

	def setIfEqual(x : Int,y: Int,z : Int,curV : T,newV : T) : T
	def setIfNotEqual(x:Int,y:Int,z:Int,curV : T,newV : T) : T

	def relativeTo( talea : ITalea[T] )(x : Int,y : Int,z : Int) : T
	def relativeTo( v : VoxelCoord )(x : Int,y : Int,z : Int) : T
	def setRelativeTo( talea : ITalea[T] )(x : Int,y : Int,z : Int,b : T)

	def getModifiedCount(x : Int , y : Int , z : Int): Int
	def getModifiedCount(v: VoxelCoord): Int

	def getModifiedCountIncludingAdjacents(v: VoxelCoord): Array[Long]
	def getModifiedCountSumIncludingAdjacents(v : VoxelCoord) : Long

	def definedAt ( v : VoxelCoord ) : Boolean
	def definedAt ( x : Int , y : Int , z : Int ) : Boolean
	def definedInRegion(region: SpatialRegion): Boolean

	def rawGetTalea (v: VoxelCoord, readOnly : Boolean): TaleaType
	def rawGetTalea (x: Int,y: Int,z: Int, readOnly : Boolean): TaleaType
	def rawGetTalea (x: Int,y: Int,z: Int): TaleaType
	def rawGetTaleaRO (v : VoxelCoord): TaleaType
	def rawGetTaleaRO (x : Int , y : Int , z : Int): TaleaType

	def rawGetTaleaOrNull ( v : VoxelCoord ) : TaleaType
	def rawGetTaleaIfExists (v : VoxelCoord) : Option[TaleaType]
	def rawGetTaleaIfExists (x : Int,y: Int,z : Int) : Option[TaleaType]

	def setTalea (talea : TaleaType)
	/**
	 * Returns all taleae that have at least one voxel in the specified region
	 */
	def allInRegionInclusiveSet ( region : SpatialRegion ): Set[TaleaType]
	def allInRegionInclusive ( region : SpatialRegion ) : List[TaleaType]

	def allTaleaPositionsInRegionInclusive ( region : SpatialRegion ) : List[VoxelCoord]

	def windowCenteredOnTaleaAt ( center: VoxelCoord , readOnly : Boolean ) : GenericTaleaGridWindow[T,TaleaType]
	def extendedWindowCenteredOnTaleaAt ( center: VoxelCoord , readOnly : Boolean ) : ExtendedGenericTaleaGridWindow[T,TaleaType]
	def byteWindowCenteredOnTaleaAt[BType <: GenericTalea[Byte] : Manifest] ( center : VoxelCoord ) : ByteTaleaGridWindow[BType]

	def windowForRegion ( region: SpatialRegion ) : GenericTaleaGridRegionWindow[T,TaleaType]

	def modificationBlock ( taleae : Iterable[TaleaType] )( stmt : => Unit )
	def modificationBlock ( window : GenericTaleaGridWindow[T,TaleaType] )( stmt : => Unit )
	def modificationBlock ( region : SpatialRegion )( stmt : => Unit )
	def modificationBlock(pos: VoxelCoord)( stmt : => Unit )
	def modificationBlock(pos: List[VoxelCoord])( stmt : => Unit )
	def modificationBlock(pos: Set[VoxelCoord])( stmt : => Unit )
	def modificationBlock(pos: mutable.HashSet[VoxelCoord])( stmt : => Unit )
	def deferredModificationBlock( taleae : Iterable[TaleaType] )( stmt : => Unit ) : Set[ITalea[_]]
//	def deferredModificationBlock( window : GenericTaleaGridWindow[T,TaleaType] )( stmt : => Unit ) : Set[ITalea[_]]
//	def deferredModificationBlock( window : ExtendedGenericTaleaGridWindow[T,TaleaType] )( stmt : => Unit ) : Set[ITalea[_]]
	def setAndNotify(pos: VoxelCoord,v : T)

	def allTaleae : Traversable[TaleaType]

	var userData : Option[Any] = None
}