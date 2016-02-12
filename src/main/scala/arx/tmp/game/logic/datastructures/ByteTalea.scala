package arx.tmp.game.logic.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/11/12
 * Time: 1:05 PM
 * Created by nonvirtualthunk
 */

import java.io.Externalizable
import java.io.ObjectInput
import java.io.ObjectOutput
import java.util

import arx.Prelude
import arx.application.Noto
import arx.core.vec.Vec3f
import arx.core.vec.Vec3i
import arx.tmp.game.logic._
import arx.core.vec.coordinates.VoxelCoord
import sun.reflect.generics.reflectiveObjects.NotImplementedException

class ByteTalea(var position:VoxelCoord,var _defaultValue:Byte) extends ITalea[Byte] with TLoggingTalea[Byte] {
	def this ()  {
		this(VoxelCoord(0,0,0),0.toByte)
	}

	var data : ByteDataContainer = new ByteDataContainer(Talea.dimension,_defaultValue)
	var _nonDefaultCount: Int = 0
	def nonDefaultCount : Int = _nonDefaultCount
	def nonDefaultCount_= (i : Int) { _nonDefaultCount = i }
	def defaultValue : Byte  = _defaultValue
	def defaultValue_= (t : Byte) { _defaultValue = t;if ( nonDefaultCount == 0 ) { data.data(0) = t } }

	var _shiftedPosition: Vec3i = (position >> Talea.dimensionPo2)
	def shiftedPosition : Vec3i = _shiftedPosition
	def shiftedPosition_= (v : Vec3i) { _shiftedPosition = v; }
	hash = Talea.hash(position.x,position.y,position.z)

	override def hashCode() = hash.toInt

	def apply (x: Int,y: Int,z: Int): Byte = {
		data(x,y,z)
	}

	def getAndDecrementToMinimumOf(x: Int, y: Int, z: Int,minimumValue:Byte) = {
		val actual = this(x,y,z)
		if ( actual > minimumValue ) {
			this(x,y,z) = (actual - 1).toByte
		}
		actual
	}

	def setIfEqual ( x : Int, y : Int,z : Int, curV : Byte , newV : Byte ) : Byte = {
		val actual = this(x,y,z)
		if ( actual == curV ) {
			this(x,y,z) = newV
		}
		actual
	}
	def setIfNotEqual ( x : Int, y : Int,z : Int, curV : Byte , newV : Byte ) : Byte = {
		val actual = this(x,y,z)
		if ( actual != curV ) {
			this(x,y,z) = newV
		}
		actual
	}
	def setIfEqualRaw ( x : Int ,y  : Int,z : Int, curV : Byte , b : Byte ) : Byte = {
		setIfEqual(x,y,z,curV,b)
	}
	def setIfGreater ( x : Int , y : Int, z : Int, newV : Byte ) : Byte = {
		synchronized {
			val rawDataIndex = data.toIndex(x,y,z)
			val actual = data(rawDataIndex)
			if ( actual < newV ) {
				if ( _nonDefaultCount == 0 ){
					if ( newV != _defaultValue ){
						_nonDefaultCount += 1

						if ( data.mask == 0x00000000 ) {
							val newData = new ByteDataContainer(Talea.dimension,_defaultValue)
							newData.allocate(_defaultValue)
							data = newData
						}

						data(rawDataIndex) = newV
						markModified(x,y,z)
					}
				}
				else{
					if ( actual == _defaultValue ){
						_nonDefaultCount += 1
					}
					else if ( newV == _defaultValue ){
						_nonDefaultCount -= 1
					}

					if ( _nonDefaultCount == 0 ){
						data = new ByteDataContainer(Talea.dimension,_defaultValue)
						data.data(0) = _defaultValue
					} else {
						data(rawDataIndex) = newV
					}

					markModified(x,y,z)
				}
			}
			actual
		}
	}
	def getBlock2x2 (x : Int,y : Int,z : Int,ret : Array[Byte]) {throw new NotImplementedException}
	override def getWithAdj(x: Int, y: Int, z: Int, ret: Array[Byte]): Unit = {
		data.getWithAdj(x,y,z,ret)
	}

	def update (x: Int,y: Int,z: Int,b: Byte){
		Prelude.posit( x >= 0 && x < Talea.dimension && y >= 0 && y < Talea.dimension && z >= 0 && z < Talea.dimension , "Update called out of bounds : " + x + "," + y + "," + z )
		synchronized{
			if ( _nonDefaultCount == 0 ){
				if ( b != _defaultValue ){
					_nonDefaultCount += 1

					if ( data.mask == 0x00000000 ) {
						data.allocate(defaultValue)
					}

					data(x,y,z) = b
					markModified(x,y,z)
					if ( isLoggingEnabled ) { Noto.finest("Logging change"); loggedModifications ::= LoggedTaleaModification(x,y,z,_defaultValue,b,_modifiedCount) }
				}
			}
			else{
				val rawIndex = data.toIndex(x,y,z)
				val cur = data(rawIndex)
				if ( b != cur ){
					if ( cur == _defaultValue ){
						_nonDefaultCount += 1
					} else if ( b == _defaultValue ){
						_nonDefaultCount -= 1
					}

					if ( nonDefaultCount == 0 ){
						data = new ByteDataContainer(Talea.dimension,_defaultValue)
					} else{
						data(rawIndex) = b
					}

					markModified(x,y,z)
					if ( isLoggingEnabled ) { Noto.finest("Logging change");loggedModifications ::= LoggedTaleaModification(x,y,z,cur,b,_modifiedCount) }
				}
			}
		}
	}

	override def setUnsafe(x:Int,y:Int,z:Int,b:Byte) {
		if ( _nonDefaultCount == 0 ){
			if ( b != _defaultValue ){
				_nonDefaultCount = 1

				if ( data.mask == 0x00000000 ) { data.allocate(defaultValue) }

				data(x,y,z) = b
				markModified(x,y,z)
				if ( isLoggingEnabled ) { Noto.finest("Logging change"); loggedModifications ::= LoggedTaleaModification(x,y,z,_defaultValue,b,_modifiedCount) }
			}
		}
		else{
			val rawIndex = data.toIndex(x,y,z)
			val cur = data(rawIndex)
			if ( b != cur ){
				if ( cur == _defaultValue ){
					_nonDefaultCount += 1
				} else if ( b == _defaultValue ){
					_nonDefaultCount -= 1
				}

				if ( nonDefaultCount == 0 ){
					data = new ByteDataContainer(Talea.dimension,_defaultValue)
				} else{
					data(rawIndex) = b
				}
				markModified(x,y,z)
				if ( isLoggingEnabled ) { Noto.finest("Logging change");loggedModifications ::= LoggedTaleaModification(x,y,z,cur,b,_modifiedCount) }
			}
		}
	}


	override def setUnsafeUnlogged(x:Int,y:Int,z:Int,b:Byte) {
		if ( _nonDefaultCount == 0 ){
			if ( b != _defaultValue ){
				_nonDefaultCount = 1

				if ( data.mask == 0x00000000 ) { data.allocate(defaultValue) }

				data(x,y,z) = b
			}
		}
		else{
			val rawIndex = data.toIndex(x,y,z)
			val cur = data(rawIndex)
			if ( b != cur ){
				if ( cur == _defaultValue ){
					_nonDefaultCount += 1
				} else if ( b == _defaultValue ){
					_nonDefaultCount -= 1
				}

				if ( nonDefaultCount == 0 ){
					data = new ByteDataContainer(Talea.dimension,_defaultValue)
				} else{
					data(rawIndex) = b
				}
			}
		}
	}

	def markModified ( i : Int ) {
		val v = data.fromIndex(i)
		markModified(v.x,v.y,v.z)
	}

	def markModified ( x : Int , y : Int , z : Int ) {
		_modifiedCount += 1
		if ( x == 0 ) { _edgeModifiedCount(Cardinals.Left) += 1 } else if ( x == Talea.dimensionM1 ) { _edgeModifiedCount(Cardinals.Right) += 1 }
		if ( y == 0 ) { _edgeModifiedCount(Cardinals.Back) += 1 } else if ( y == Talea.dimensionM1 ) { _edgeModifiedCount(Cardinals.Front) += 1 }
		if ( z == 0 ) { _edgeModifiedCount(Cardinals.Bottom) += 1 } else if ( z == Talea.dimensionM1 ) { _edgeModifiedCount(Cardinals.Top) += 1 }
	}

	def setAll (b: Byte){
		synchronized {
			_defaultValue = b
			nonDefaultCount = 0
			data.deallocate()
			_modifiedCount += 1
			_edgeModifiedCount = _edgeModifiedCount.map ( _ + 1 )
		}
	}

	def areAll (b: Byte): Boolean = { nonDefaultCount == 0 && _defaultValue == b }
	def areNone (b: Byte): Boolean = { _defaultValue == b && nonDefaultCount == Talea.dimension*Talea.dimension*Talea.dimension }

	override def x: Int = position.x
	override def y: Int = position.y
	override def z: Int = position.z

	override def memoryUsage: Int = {
		data.memoryUsage
	}
	def getShiftedPosition = shiftedPosition

	override def containsPoint ( v : Vec3f ) : Boolean = {
		(v.x >= position.x && v.y >= position.y && v.z >= position.z &&
		 v.x < position.x + Talea.dimension && v.y < position.y + Talea.dimension && v.z < position.z + Talea.dimension)
	}

//	@throws(classOf[IOException])
//	override def writeExternal(p1: ObjectOutput) {
//		p1.writeObject(position)
//		p1.writeObject(data)
//		p1.writeObject(_defaultValue)
//		p1.writeInt(nonDefaultCount)
//		if ( nonDefaultCount > 0 ) {
////			p1.writeObject(nonDefaultCountByZ)
////			p1.writeObject(nonDefaultCountByXY)
//		}
//		p1.writeInt(_modifiedCount)
//		p1.writeObject(_edgeModifiedCount)
//	}
//	@throws(classOf[IOException])
//	override def readExternal(p1: ObjectInput) {
//		position = p1.readObject.asInstanceOf[VoxelCoord]
//		data = p1.readObject.asInstanceOf[ByteDataContainer]
//		_defaultValue = p1.readObject.asInstanceOf[Byte]
//
//		nonDefaultCount = p1.readInt
//		if ( nonDefaultCount > 0 ) {
////			_nonDefaultCountByZ = p1.readObject.asInstanceOf[Array[Short]]
////			nonDefaultCountByXY = p1.readAs[Array[Byte]]
//		}
//		_modifiedCount = p1.readInt
//		_edgeModifiedCount = p1.readObject.asInstanceOf[Array[Int]]
//		hash = Talea.hash(position.x,position.y,position.z)
//		shiftedPosition = (position >> Talea.dimensionPo2)
//	}

	override def toString : String = {
		"Talea " + position
	}
}

class ByteDataContainer(var dimension:Int,defaultValue:Byte) extends Externalizable {
	def this () { this(0,0.toByte) }
	var dimensionPo2:Int = (math.log(dimension) / math.log(2)).toInt
	var dimensionPo22:Int = dimensionPo2 + dimensionPo2
	var data: Array[Byte] = Array.ofDim[Byte](1)
	data(0) = defaultValue
	var mask = 0x00000000
	def rawData = data
	override def writeExternal ( out : ObjectOutput ) {
		out.writeInt(dimension)
		out.writeInt(data.size)
		if ( data.size > 1 ) {
			val compressedData = Compression.compress(data.asInstanceOf[Array[Byte]],true)
			out.writeObject(compressedData)
		}
		out.writeInt(mask)
	}
	override def readExternal ( in : ObjectInput ) {
		dimension = in.readInt
	    dimensionPo2 = (math.log(dimension) / math.log(2)).toInt
		dimensionPo22 = dimensionPo2 + dimensionPo2
		
		val uncompressedSize = in.readInt
		if ( uncompressedSize > 1 ) {
			val compressedData = in.readAs[Array[Byte]]
			data = new Array[Byte](uncompressedSize)
			Compression.uncompress(compressedData,data.asInstanceOf[Array[Byte]])
		} else {
			data = new Array[Byte](1)
		}
		mask = in.readInt
	}
	
	@volatile def allocate (){
		data = new Array[Byte](dimension * dimension * dimension)
		mask = 0xffffffff
	}
	def allocate (defaultValue : Byte) {
		allocate()
		//If defaultValue is not the built-in default
		if ( data(0) != defaultValue ) {
			var i = 0;
			val size = dimension * dimension * dimension
			while ( i < size ) {
				data(i) = defaultValue
				i += 1
			}
		}
	}
	def deallocate (){
		mask = 0x00000000
		data = new Array[Byte](1)
		data(0) = defaultValue
	}

	def compress (keepOriginal: Boolean = false){
		throw new IllegalStateException("Wrong exception, point is we don't compress/uncompress these days")
	}
	def uncompress (){
		throw new IllegalStateException("Wrong exception, point is we don't compress/uncompress these days")
	}


//	def toIndex ( x : Int , y : Int , z : Int ) = ((z << dimensionPo22) + (y << dimensionPo2) + x)
	def toIndex ( x : Int , y : Int , z : Int ) = ((x << dimensionPo22) + (y << dimensionPo2) + z)
//	def fromIndex ( i : Int ) = Vec3i( i & (dimension-1) , (i - ((i >> dimensionPo22) << dimensionPo22)) >> dimensionPo2 , i >> dimensionPo22  )
	def fromIndex ( i : Int ) = Vec3i( i >> dimensionPo22 , (i - ((i >> dimensionPo22) << dimensionPo22)) >> dimensionPo2 , i & (dimension-1)  )

//	def zFromIndex ( i : Int ) : Int = i >> dimensionPo22
//	def xFromIndex ( i : Int ) : Int = i & (dimension-1)
//	def yFromIndex ( i : Int ) : Int = (i - ((i >> dimensionPo22) << dimensionPo22)) >> dimensionPo2
	def xFromIndex ( i : Int ) : Int = i >> dimensionPo22
	def zFromIndex ( i : Int ) : Int = i & (dimension-1)
	def yFromIndex ( i : Int ) : Int = (i - ((i >> dimensionPo22) << dimensionPo22)) >> dimensionPo2


//	def apply (x: Int, y: Int, z: Int): Byte = data(((z << dimensionPo22) + (y << dimensionPo2) + x) & mask)
	def apply (x: Int, y: Int, z: Int): Byte = data(((x << dimensionPo22) + (y << dimensionPo2) + z) & mask)
	def apply (i:Int): Byte = data(i & mask)
//	def update (x: Int,y: Int,z: Int,b: Byte){ data((z << dimensionPo22) + (y << dimensionPo2) + x) = b }
	def update (x: Int,y: Int,z: Int,b: Byte){ data((x << dimensionPo22) + (y << dimensionPo2) + z) = b }
	def update (i:Int,b: Byte){ data(i) = b }

	def getBlock2x2(x : Int,y: Int,z: Int,ret : Array[Byte]) {throw new NotImplementedException}
	def getWithAdj(x:Int,y:Int,z:Int,ret:Array[Byte]) {
		if (mask == 0x00000000) {
			util.Arrays.fill(ret,data(0))
		} else {
			ret (0) = data (toIndex(x-1,y,z))
			ret (1) = data (toIndex(x,y-1,z))
			ret (2) = data (toIndex(x,y,z-1))
			ret (3) = data (toIndex(x+1,y,z))
			ret (4) = data (toIndex(x,y+1,z))
			ret (5) = data (toIndex(x,y,z+1))
			ret (6) = data (toIndex(x,y,z))
		}
	}

	def memoryUsage: Int = {
		var sum = 0
		if ( data != null ){ sum += data.length }
		sum
	}
}