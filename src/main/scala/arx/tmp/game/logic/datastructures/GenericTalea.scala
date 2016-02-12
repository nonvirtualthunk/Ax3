package arx.tmp.game.logic.datastructures

import arx.Prelude
import arx.Prelude.toArxClass
import arx.application.Noto
import arx.core.vec.Vec3i._
import arx.core.vec.Vec3f
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.VoxelCoord




@SerialVersionUID(1L)
class GenericTalea[@specialized(Byte,Short,Int) T : Manifest] extends ITalea[T] with TLoggingTalea[T] {
	def createDataContainer() = new DataContainer[T](Talea.dimension,_defaultValue,getComponentType.asInstanceOf[Class[T]])
	def this (pos : VoxelCoord, defVal : T) {
		this()
		_defaultValue = defVal
		position = pos
	}
	def this (pos : VoxelCoord) {
		this()
		position = pos
	}

	var _position : VoxelCoord = VoxelCoord.Sentinel
	def position = _position
	protected[datastructures] def position_= (pos : VoxelCoord) {
		_position = pos
		_shiftedPosition = pos >> Talea.dimensionPo2
		hash = Talea.hash(position.x,position.y,position.z)
	}
	var _defaultValue : T = _
	var data : DataContainer[T] = createDataContainer()
	var _nonDefaultCount: Int = 0
	def nonDefaultCount : Int = _nonDefaultCount
	def nonDefaultCount_= (i : Int) { _nonDefaultCount = i }
	def defaultValue : T  = _defaultValue
	def defaultValue_= (t : T) { _defaultValue = t;if ( nonDefaultCount == 0 ) { data.rawData(0) = t } }

	var _shiftedPosition: Vec3i = VoxelCoord.Sentinel
	def shiftedPosition : Vec3i = _shiftedPosition
	def shiftedPosition_= (v : Vec3i) { _shiftedPosition = v; }

	def getComponentType = manifest[T].runtimeClass
	def withData (d : DataContainer[T]) = { data = d ; this }

	override def equals ( a : Any ) : Boolean = {
		a match {
			case t: GenericTalea[T] => {
				t.position.equals(position) &&
				t._defaultValue == this._defaultValue &&
				t.nonDefaultCount == this.nonDefaultCount &&
				t.shiftedPosition.equals(this.shiftedPosition) &&
				t._modifiedCount == this._modifiedCount &&
				t.hash == this.hash &&
				t.data.equals(this.data)
			}
			case _ => false
		}
	}


	override def hashCode() = hash

	def apply (x: Int,y: Int,z: Int): T = {
		data(x,y,z)
	}

	def getAndDecrementToMinimumOf(x: Int, y: Int, z: Int,minimumValue : T) = throw new UnsupportedOperationException

	def setIfEqual ( x : Int, y : Int,z : Int, curV : T , newV : T ) : T = {
		val actual = this(x,y,z)
		if ( actual == curV ) {
			this(x,y,z) = newV
		}
		actual
	}

	def setIfNotEqual ( x : Int , y : Int , z : Int , curV : T , newV : T ) : T = {
		val actual = this(x,y,z)
		if ( actual != curV ) {
			this(x,y,z) = newV
		}
		actual
	}

	def setIfEqualRaw ( x : Int ,y  : Int,z : Int, curV : T , b : T ) : T = {
		val actual = this(x,y,z)
		if ( actual == curV && curV != b) {
			if ( _nonDefaultCount == 0 ){
				if ( b != _defaultValue ){
					_nonDefaultCount += 1

					if ( data.mask == 0x00000000 ) {
						val newData = createDataContainer()
						newData.allocate(_defaultValue)
						data = newData
					}

					data(x,y,z) = b
					markModified(x,y,z)
				}
			}
			else{
				if ( curV == _defaultValue ){
					_nonDefaultCount += 1
				}
				else if ( b == _defaultValue ){
					_nonDefaultCount -= 1
				}

				if ( _nonDefaultCount == 0 ){
					data = createDataContainer()
					data.rawData(0) = _defaultValue
				} else {
					data(x,y,z) = b
				}

				markModified(x,y,z)
			}
		}
		actual
	}

	def setIfGreater ( x : Int , y : Int, z : Int, newV : T ) : T = {
		throw new UnsupportedOperationException("Java stupid generics means we can't use <, >, etc in the generic talea")
	}

	def getBlock2x2 (x : Int,y : Int,z : Int,ret : Array[T]) {
		if ( nonDefaultCount != 0 ) {
			data.getBlock2x2(x,y,z,ret)
		} else {
			ret(0) = _defaultValue
			ret(1) = _defaultValue
			ret(2) = _defaultValue
			ret(3) = _defaultValue
			ret(4) = _defaultValue
			ret(5) = _defaultValue
			ret(6) = _defaultValue
			ret(7) = _defaultValue
		}
	}
	def getWithAdj (x:Int,y:Int,z:Int,ret : Array[T]) {
		throw new UnsupportedOperationException
	}

	def update (x: Int,y: Int,z: Int,b: T){
		Prelude.posit( x >= 0 && x < Talea.dimension && y >= 0 && y < Talea.dimension && z >= 0 && z < Talea.dimension , "Update called with invalid bounds" )
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
				val index = data.toIndex(x,y,z)
				val cur = data.getByIndexUnsafe(index)
				if ( b != cur ){
					if ( cur == _defaultValue ){
						_nonDefaultCount += 1
					}
					else if ( b == _defaultValue ){
						_nonDefaultCount -= 1
					}

					if ( nonDefaultCount == 0 ){
						data = createDataContainer()
					}
					else{
						data.setByIndex(index,b)
					}

					markModified(x,y,z)
					if ( isLoggingEnabled ) { Noto.finest("Logging change");loggedModifications ::= LoggedTaleaModification(x,y,z,cur,b,_modifiedCount) }
				}
			}
		}
	}

	override def setUnsafe (x: Int,y: Int,z: Int,b: T){
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
			val index = data.toIndex(x,y,z)
			val cur = data.getByIndexUnsafe(index)
			if ( b != cur ){
				if ( cur == _defaultValue ){
					_nonDefaultCount += 1
				}
				else if ( b == _defaultValue ){
					_nonDefaultCount -= 1
				}

				if ( nonDefaultCount == 0 ){
					data = createDataContainer()
				}
				else{
					data.setByIndex(index,b)
				}

				markModified(x,y,z)
				if ( isLoggingEnabled ) { Noto.finest("Logging change");loggedModifications ::= LoggedTaleaModification(x,y,z,cur,b,_modifiedCount) }
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

	def setAll (b: T){
		synchronized {
			_defaultValue = b
			nonDefaultCount = 0
			data.deallocate()
			_modifiedCount += 1
			_edgeModifiedCount = _edgeModifiedCount.map ( _ + 1 )
		}
	}

	def areAll (b: T): Boolean = { nonDefaultCount == 0 && _defaultValue == b }
	def areNone (b: T): Boolean = { _defaultValue == b && nonDefaultCount == Talea.dimension*Talea.dimension*Talea.dimension }

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
//		p1.writeObject(storedManifest)
//		p1.writeObject(position)
//		p1.writeObject(data)
//		p1.writeObject(_defaultValue)
//		p1.writeInt(nonDefaultCount)
//		p1.writeInt(_modifiedCount)
//		p1.writeObject(_edgeModifiedCount)
//	}
//	@throws(classOf[IOException])
//	override def readExternal(p1: ObjectInput) {
//		storedManifest = p1.readAs[Manifest[T]]
//		position = p1.readAs[VoxelCoord]
//		data = p1.readAs[DataContainer[T]]
//		_defaultValue = p1.readAs[T]
//
//		nonDefaultCount = p1.readInt
//		_modifiedCount = p1.readInt
//		_edgeModifiedCount = p1.readAs[Array[Int]]
//		hash = Talea.hash(position.x,position.y,position.z)
//		shiftedPosition = (position >> Talea.dimensionPo2)
//	}

	override def toString : String = {
		"Talea " + position
	}
}


@SerialVersionUID(1L)
class DataContainer[@specialized(Byte,Short,Int) T](val dimension:Int,val defaultValue:T,val componentClass : Class[T]) {
	var dimensionPo2:Int = (math.log(dimension) / math.log(2)).toInt
	var dimensionPo22:Int = dimensionPo2 + dimensionPo2
	private[datastructures] var data: Array[T] = { val d = componentClass.newArray(1);d(0) = defaultValue;d }
	var mask = 0x00000000

	def rawData = data
	def setData (d : Array[T]) { data = d}

//	import arx.serialization.ArxInputStream._
//	override def writeExternal ( out : ObjectOutput ) {
//		out.writeObject(componentClass)
//		out.writeInt(dimension)
//		// could perform compression here in certain situations, if desired
//		out.writeObject(data)
//		out.writeInt(mask)
//	}
//	override def readExternal ( in : ObjectInput ) {
//		componentClass = in.readAs[Class[T]]
//		dimension = in.readInt
//	    dimensionPo2 = (math.log(dimension) / math.log(2)).toInt
//		dimensionPo22 = dimensionPo2 + dimensionPo2
//		data = in.readAs[Array[T]]
//		mask = in.readInt
//
//		if ( data == null ) {
//			Noto.error("Talea somehow has null data in data container on read in, patching up")
//			data = componentClass.newArray(1)
//		}
//	}


	def compressed: Boolean = false

	@volatile def allocate (){
		data = componentClass.newArray(dimension * dimension * dimension)
		mask = 0xffffffff
	}
	def allocate (defaultValue : T) {
		allocate()
		//If defaultValue is not the built-in default
		if ( data(0) != defaultValue ) {
			var i = 0
			val size = dimension * dimension * dimension

			while ( i < size ) {
				data(i) = defaultValue
				i += 1
			}
		}
	}
	def deallocate (){
		mask = 0x00000000
		data = componentClass.newArray(1)
		data(0) = defaultValue
	}

	def compress (keepOriginal: Boolean = false){
		throw new IllegalStateException("Wrong exception, point is we don't compress/uncompress these days")
	}
	def uncompress (){
		throw new IllegalStateException("Wrong exception, point is we don't compress/uncompress these days")
	}


	@inline def toIndex ( x : Int , y : Int , z : Int ) = ((z << dimensionPo22) + (y << dimensionPo2) + x)
	//Test fromIndex, not sure how correct this is
	@inline def fromIndex ( i : Int ) = Vec3i( i & (dimension-1) , (i - ((i >> dimensionPo22) << dimensionPo22)) >> dimensionPo2 , i >> dimensionPo22  )
	@inline def zFromIndex ( i : Int ) : Int = i >> dimensionPo22
	@inline def xFromIndex ( i : Int ) : Int = i & (dimension-1)
	@inline def yFromIndex ( i : Int ) : Int = (i - ((i >> dimensionPo22) << dimensionPo22)) >> dimensionPo2

	@inline def apply (x: Int, y: Int, z: Int): T = data(((z << dimensionPo22) + (y << dimensionPo2) + x) & mask)
	@inline def update (x: Int,y: Int,z: Int,b: T){ data((z << dimensionPo22) + (y << dimensionPo2) + x) = b }

	@inline def getUnsafe(x: Int, y: Int, z: Int): T = data((z << dimensionPo22) + (y << dimensionPo2) + x)
	@inline def getByIndexUnsafe(i:Int) = data(i)
	@inline def setByIndex(i:Int,b:T) { data(i) = b }

	def getBlock2x2(x : Int,y: Int,z: Int,ret : Array[T]) {
		val h1 = ((z << dimensionPo22) + (y << dimensionPo2) + x)
		ret(0) = data(h1 & mask)
		ret(1) = data((h1 + 1) & mask)
		ret(2) = data((h1 + dimension) & mask)
		ret(3) = data((h1 + dimension + 1) & mask)
		val h2 = (((z+1) << dimensionPo22) + (y << dimensionPo2) + x)
		ret(4) = data(h2 & mask)
		ret(5) = data((h2 + 1) & mask)
		ret(6) = data((h2 + dimension) & mask)
		ret(7) = data((h2 + dimension + 1) & mask)
	}

	def memoryUsage: Int = {
		var sum = 0
		if ( data != null ){ sum += data.length }
		sum
	}

	override def equals ( a : Any ) : Boolean = {
		a match {
			case d : DataContainer[T] => {
				this.dimension == d.dimension &&
				this.dimensionPo2 == d.dimensionPo2 &&
				this.dimensionPo22 == d.dimensionPo22 &&
				this.mask == d.mask &&
				(if ( this.data == null ) { d.rawData == null } else { (0 until this.data.length).forall( { i => this.data(i) == d.rawData(i) } ) })
			}
			case _ => false
		}
	}
}











