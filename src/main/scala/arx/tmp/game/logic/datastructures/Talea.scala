package arx.tmp.game.logic.datastructures

import arx.core.vec.ReadVec3i
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.VoxelCoord

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 6/8/11
 * Time: 8:21 PM
 * Created by nonvirtualthunk
 */

//
//class Talea(pos: Vec3i,defaultValue: Byte = 0.toByte) extends GenericTalea[Byte](pos,defaultValue){
//	def this() {
//		this(Vec3i(0,0,0))
//	}
//
//	def setIfLessThan(x : Int,y: Int,z: Int,b: Byte) : Boolean = {
//		if ( this(x,y,z) < b ) {
//			this(x,y,z) = b
//			true
//		}
//		false
//	}
//}


/**
 * Default constructor for serialization only
 */
//class Talea(var position:Vec3i) extends ITalea[Byte] {
//	def this () { this(Vec3i(0,0,0)) }
//	var data = new DataContainer[Byte](Talea.dimension)
//	var defaultValue: Byte = 0
//	var nonDefaultCount: Int = 0
//	var shiftedPosition: Vec3i = (position >> Talea.dimensionPo2)
//	hash = Talea.hash(position.x,position.y,position.z)
//
//	override def equals ( a : Any ) : Boolean = {
//		a match {
//			case t: Talea => {
//				t.position.equals(position) &&
//				t.defaultValue == this.defaultValue &&
//				t.nonDefaultCount == this.nonDefaultCount &&
//				t.shiftedPosition.equals(this.shiftedPosition) &&
//				t._modifiedCount == this._modifiedCount &&
//				t.hash == this.hash &&
//				t.data.equals(this.data)
//			}
//			case _ => false
//		}
//	}
//
//	def apply (x: Int,y: Int,z: Int): Byte = {
//		if ( nonDefaultCount != 0 ){
//			//synchronized {
//				data(x,y,z)
//			//}
//		}
//		else{
//			defaultValue
//		}
//	}
//	def apply (i: Int): Byte = {
//		if ( nonDefaultCount != 0 ){
//			//synchronized {
//				data(i)
//			//}
//		}
//		else{
//			defaultValue
//		}
//	}
//	def update (x: Int,y: Int,z: Int,b: Byte){
//		synchronized{
//			if ( nonDefaultCount == 0 ){
//				if ( b != defaultValue ){
//					nonDefaultCount += 1
//					data.allocate()
//					data(x,y,z) = b
//					_modifiedCount += 1
//				}
//			}
//			else{
//				val cur = data(x,y,z)
//				if ( b != cur ){
//					if ( cur == defaultValue ){
//						nonDefaultCount += 1
//					}
//					else if ( b == defaultValue ){
//						nonDefaultCount -= 1
//					}
//
//					if ( nonDefaultCount == 0 ){
//						data.deallocate()
//					}
//					else{
//						data(x,y,z) = b
//					}
//
//					_modifiedCount += 1
//				}
//			}
//		}
//	}
//	def update (i: Int,b: Byte){
//		synchronized{
//			if ( nonDefaultCount == 0 ){
//				if ( b != defaultValue ){
//					nonDefaultCount += 1
//					data.allocate()
//					data(i) = b
//					_modifiedCount += 1
//				}
//			}
//			else{
//				val cur = data(i)
//				if ( b != cur ){
//					if ( cur == defaultValue ){
//						nonDefaultCount += 1
//					}
//					else if ( b == defaultValue ){
//						nonDefaultCount -= 1
//					}
//
//					if ( nonDefaultCount == 0 ){
//						data.deallocate()
//					}
//					else{
//						data(i) = b
//					}
//					_modifiedCount += 1
//				}
//			}
//		}
//	}
//
//	def setAll (b: Byte){
//		defaultValue = b
//		nonDefaultCount = 0
//		data.deallocate()
//		_modifiedCount += 1
//	}
//
//	def areAll (b: Byte): Boolean = { nonDefaultCount == 0 && defaultValue == b }
//
//	override def x: Int = position.x
//	override def y: Int = position.y
//	override def z: Int = position.z
//
//	def memoryUsage: Int = {
//		data.memoryUsage
//	}
//
//	override def hashCode() = hash
//
//
//
//	def getShiftedPosition = shiftedPosition
//
//	override def containsPoint ( v : Vec3f ) : Boolean = {
//		(v.x >= position.x && v.y >= position.y && v.z >= position.z &&
//		 v.x < position.x + Talea.dimension && v.y < position.y + Talea.dimension && v.z < position.z + Talea.dimension)
//	}
//
//	@throws(classOf[IOException])
//	override def writeExternal(p1: ObjectOutput) {
//		p1.writeObject(position)
//		p1.writeObject(data)
//		p1.writeByte(defaultValue)
//		p1.writeInt(nonDefaultCount)
//		p1.writeObject(shiftedPosition)
//		p1.writeInt(_modifiedCount)
//		p1.writeInt(hash)
////		p1.writeObject(features)
//	}
//	@throws(classOf[IOException])
//	override def readExternal(p1: ObjectInput) {
//		position = p1.readAs[Vec3i]
//		data = p1.readAs[DataContainer[Byte]]
//		defaultValue = p1.readByte
//		nonDefaultCount = p1.readInt
//		shiftedPosition = p1.readAs[Vec3i]
//		_modifiedCount = p1.readInt
//		hash = p1.readInt
//		//features = p1.readAs[List[TerrainFeature]]
//	}
//}

object Talea{
	val dimensionPo2 = 5//:Int = (Math.log(dimension) / Math.log(2)).toInt
	val dimension = 1 << dimensionPo2
	val dimensionf = dimension.toFloat
	val dimensionM1 = dimension - 1

	def hash (x: Int,y: Int,z: Int): Int = {
		((x >> dimensionPo2) << 20) | ((y >> dimensionPo2) << 10) | (z >> dimensionPo2)
	}
	def hashPreShifted(x: Int, y: Int, z: Int): Int = {
		( (x) << 20) | ((y) << 10) | (z)
	}

	val cardinals = Array.ofDim[ReadVec3i](7)
	cardinals(Cardinals.Left) = Vec3i(-dimension,0,0)
	cardinals(Cardinals.Right) = Vec3i(+dimension,0,0)
	cardinals(Cardinals.Back) = Vec3i(0,-dimension,0)
	cardinals(Cardinals.Front) = Vec3i(0,+dimension,0)
	cardinals(Cardinals.Bottom) = Vec3i(0,0,-dimension)
	cardinals(Cardinals.Top) = Vec3i(0,0,+dimension)
	cardinals(Cardinals.Center) = Vec3i(0,0,0)
}

object SentinelTalea extends GenericTalea[Byte](VoxelCoord(-1,-1,-1),0.toByte){
	override def apply (x: Int,y: Int,z: Int): Byte = { -1 }
	def apply (): Byte = { -1 }
	override def update (x: Int,y: Int,z: Int,b: Byte){ }
}