package arx.tmp.game.logic.datastructures

import arx.core.vec.ReadVec3i
import arx.core.vec.Vec3f
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.traits.Cell3D

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/16/11
 * Time: 10:49 AM
 * Created by nonvirtualthunk
 */

trait TVoxelView[@specialized(Byte,Short,Int) T] {
	def apply (x: Int,y : Int,z: Int): T
}

trait TUpdatableVoxelView[@specialized(Byte,Short,Int) T] extends TVoxelView[T] {
	def update (x: Int,y : Int,z: Int,t: T)
}

trait TVoxelStore[@specialized(Byte,Short,Int) T] extends TUpdatableVoxelView[T] {}

trait TExtendedVoxelStore[@specialized(Byte,Short,Int) T] extends TVoxelStore[T]{}
trait TUpdatableExtendedVoxelView[@specialized(Byte,Short,Int) T] extends TUpdatableVoxelView[T] with TExtendedVoxelView[T]{}
trait TExtendedVoxelView[@specialized(Byte,Short,Int) T] extends TVoxelView[T]{}
trait TInfiniteVoxelView[@specialized(Byte,Short,Int) T] extends TVoxelView[T]{
	def apply(v : ReadVec3i): T = this.apply(v.x,v.y,v.z)
}
trait TInfiniteVoxelStore[@specialized(Byte,Short,Int) T] extends TInfiniteVoxelView[T] with TVoxelStore[T]{
	def update(v:ReadVec3i,b: T){ this.update(v.x,v.y,v.z,b) }
}


trait ITalea[@specialized(Byte,Short,Int) T] extends TVoxelStore[T] with Cell3D {
	@volatile var _modifiedCount: Int = 0
	var _edgeModifiedCount : Array[Int] = Array.ofDim[Int](6) //lx hx , ly hy , lz hy
	var hash: Int = 0
	var isLoggingEnabled = false

	def defaultValue : T
	def defaultValue_= (t : T)

	protected[datastructures] def position_= (pos : VoxelCoord)

	/** Gets the value at the given coordinates and decrements it, to a minimum of 0 */
	def getAndDecrementToMinimumOf (x:Int,y:Int,z:Int,minimumValue : T):T
	def getBlock2x2 (x:Int,y:Int,z:Int,ret:Array[T]);
	def getWithAdj (x:Int,y:Int,z:Int,ret:Array[T])
	def setIfEqual ( x : Int, y : Int,z : Int, curV : T , newV : T ) : T
	def setIfNotEqual ( x : Int, y : Int,z : Int, curV : T, newV : T ) : T
	def setIfEqualRaw ( x : Int ,y  : Int,z : Int, curV : T , newV : T ) : T
	/** Sets voxel to v, if v > current, returns current */
	def setIfGreater ( x : Int , y : Int, z : Int, newV : T ) : T

	def setUnsafe ( x : Int , y : Int , z : Int , newV : T ) { update(x,y,z,newV) }
	def setUnsafeUnlogged ( x : Int , y : Int , z : Int , newV : T ) { update(x,y,z,newV) }

	def setAll (b: T)
	def areAll (b: T): Boolean
	def areNone (b : T): Boolean

	def memoryUsage: Int = { 0 }

	def incrementModifiedCount () { _modifiedCount += 1 }
	def modifiedCount: Int = _modifiedCount
	def modifiedCount_=(i: Int){ _modifiedCount = i }

	def lockModifiedCount () { }
	def unlockModifiedCount () { }

	def edgeModifiedCount(i: Int): Int = _edgeModifiedCount(i)
	def edgeModifiedCount : Array[Int] =  _edgeModifiedCount

	def containsPoint ( v : Vec3f ) : Boolean = {
		(v.x >= position.x && v.y >= position.y && v.z >= position.z &&
		 v.x < position.x + Talea.dimension && v.y < position.y + Talea.dimension && v.z < position.z + Talea.dimension)
	}

	def enableLogging () { isLoggingEnabled = true }
	def disableLogging () { isLoggingEnabled = false }

	override def hashCode() = hash
}