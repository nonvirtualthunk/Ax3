package arx.tmp.game.logic.datastructures

import arx.Prelude
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.VoxelCoord

/**
 *
 */


trait TTaleaGridWindow[@specialized(Byte,Short,Int) T,TaleaType] extends TUpdatableExtendedVoxelView[T] {
	/** Sets voxel to v, if v > current, returns current */
	def setIfGreater(x: Int, y: Int, z: Int, v: T) : T

	def center : ReadVec3i
	def centerTalea : TaleaType
	def aboveCenterTalea : TaleaType
	def belowCenterTalea : TaleaType
	def taleaContaining ( x : Int , y : Int , z : Int ) : TaleaType
	def taleaContaining( v : ReadVec3i ) : TaleaType = taleaContaining(v.x,v.y,v.z)
	def lock ()
	def unlock ()
	def taleae : Array[TaleaType]
	def setIfEqual ( x : Int, y : Int,z : Int, curV : T , newV : T ) : T
	def setIfEqualRaw ( x : Int ,y  : Int,z : Int, curV : T , newV : T ) : T

	def withFallback : TTaleaGridWindow[T,TaleaType]
}

/**
 * In the window's coordinates, (0,0,0) is the bottom left of the center talea
 */
@SerialVersionUID(1L)
class GenericTaleaGridWindow[@specialized(Byte,Short,Int) T, TaleaType <: ITalea[T] : Manifest] ( grid : TTaleaGrid[T,TaleaType] , val center: VoxelCoord , var readOnly : Boolean = false ) extends
			TTaleaGridWindow[T,TaleaType] with Serializable
{
	protected val dimensionPo2 = Talea.dimensionPo2
	protected val dimensionm1 = Talea.dimension - 1
	protected val dimension = Talea.dimension
	protected val dimensionT2 = Talea.dimension * 2
	val _rawTaleae = new Array[TaleaType](43) //we have fake dimensions of 4, so we can use shifts, but the maxium we can get is 2+2*4+2*16
	def rawTaleae( x : Int ,y : Int , z : Int ) : TaleaType = {
		val d1 = dimensionPo2
		val rx = (x >> d1) + 1
		val ry = (y >> d1) + 1
		val rz = (z >> d1) + 1

		rawTaleae( rx + (ry << 2) + (rz << 4) )
	}
	def rawTaleae( index : Int ) : TaleaType = {
		if ( _rawTaleae(index) != null ) { _rawTaleae(index) }
		else {
			_rawTaleae(index) = grid.rawGetTalea(
				center.x + GenericTaleaGridWindow.offsets(index).x,
				center.y + GenericTaleaGridWindow.offsets(index).y,
				center.z + GenericTaleaGridWindow.offsets(index).z,
				readOnly
			)
			_rawTaleae(index)
		}
	}
	def setIfEqual(x: Int, y: Int, z: Int, curV: T, newV: T) : T = {
		val t = rawTaleae(x,y,z)
		t.setIfEqual((x + dimensionT2) & dimensionm1,(y + dimensionT2) & dimensionm1,(z + dimensionT2) & dimensionm1,curV,newV)
	}
	def setIfEqualRaw(x: Int, y: Int, z: Int, curV: T, newV: T) : T = {
		val t = rawTaleae(x,y,z)
		t.setIfEqualRaw((x + dimensionT2) & dimensionm1,(y + dimensionT2) & dimensionm1,(z + dimensionT2) & dimensionm1,curV,newV)
	}
	def setIfGreater(x: Int, y: Int, z: Int, v: T) : T = {
		val t = rawTaleae(x,y,z)
		t.setIfGreater((x + dimensionT2) & dimensionm1,(y + dimensionT2) & dimensionm1,(z + dimensionT2) & dimensionm1,v)
	}

	var _taleae : Array[TaleaType] = null
	val centerIndex = (1) + ((1) << 2) + ((1) << 4)

	def initTaleae () {
		var incrementor = 0
		var z = -1;
		while ( z <= 1 ) {
			var y = -1
			while ( y <= 1 ) {
				var x = -1
				while ( x <= 1 ) {
					val index = (x + 1) + ((y + 1) << 2) + ((z + 1) << 4)
					_taleae(incrementor) = rawTaleae(index)
					Prelude.posit(_taleae(incrementor) != null,"Null _taleae : " + x + "," + y + "," + z)
					incrementor += 1
				x += 1}
			y += 1}
		z += 1}
		Prelude.posit(incrementor == 27,"Incrementor did not get as far as expected")
	}

	def taleae : Array[TaleaType] = {
		if ( _taleae == null ) {
			_taleae = new Array[TaleaType](27) //Keep an easily iterable array in z/y/x ordering
			initTaleae()
		}
		_taleae
	}

	def centerTalea : TaleaType = { rawTaleae(centerIndex) }
	def aboveCenterTalea : TaleaType = { rawTaleae( 1 + (1 << 2) + (2 << 4) ) }
	def belowCenterTalea : TaleaType = { rawTaleae( 1 + (1 << 2) /* + (0 << 4) */ ) }
	def taleaContaining ( x : Int , y : Int , z : Int ) : TaleaType = { rawTaleae(x,y,z) }

	def relativeTo( v : ReadVec3i )( x : Int , y : Int , z : Int ) : T = {
		this((v.x - center.x) + x,(v.y - center.y) + y,(v.z - center.z) + z)
	}

	def apply ( x: Int, y: Int, z: Int ): T = {
//		if ( x >= 0 && x < dimension && y >= 0 && y < dimension && z >= 0 && z < dimension ) {
//		if ( (x >> dimensionPo2) == 0 && (y >> dimensionPo2) == 0 && (z >> dimensionPo2) == 0 ) {
//			rawTaleae(centerIndex)(x,y,z)
//		} else {
			val t = rawTaleae(x,y,z)
			val d = dimension
			val d1 = dimensionm1
			t((x + d) & d1,(y + d) & d1,(z + d) & d1)
//		}
	}

	def getBlock2x2 ( x : Int , y : Int , z : Int , result : Array[T] ) {
		val tp2 = Talea.dimensionPo2
		if ( (x >> tp2) == ((x + 1) >> tp2) && (y >> tp2) == ((y + 1) >> tp2) && (z >> tp2) == ((z + 1) >> tp2) ) {
			val t = rawTaleae(x,y,z)
			Prelude.posit( t != null , "Attempting to access invalid voxel in window : " + x + "," + y + "," + z )

			t.getBlock2x2((x + dimension) & dimensionm1,(y + dimension) & dimensionm1,(z + dimension) & dimensionm1,result)
		} else {
			result(0) = this(x,y,z);result(1) = this(x+1,y,z);result(2) = this(x,y+1,z);result(3) = this(x+1,y+1,z)
			result(4) = this(x,y,z+1);result(5) = this(x+1,y,z+1);result(6) = this(x,y+1,z+1);result(7) = this(x+1,y+1,z+1)
		}
	}

	def update ( x: Int, y: Int, z: Int , b: T) {
//		if ( x >= 0 && x < dimension && y >= 0 && y < dimension && z >= 0 && z < dimension ) {
//			rawTaleae(centerIndex)(x,y,z) = b
//		} else {
			val t = rawTaleae(x,y,z)
			Prelude.posit( t != null , "Attempting to access invalid voxel in window : " + x + "," + y + "," + z )
			t((x + dimension) & dimensionm1,(y + dimension) & dimensionm1,(z + dimension) & dimensionm1) = b
//		}
	}

	def lock () {
		for ( t <- taleae ) {
			t match {
				case l : TLockable => {
					if ( l.locked ) { println("Waiting on lock for " + l) }
					l.lock()
				}
			}
		}
	}

	def unlock () {
		for ( t <- taleae.reverse ) {
			t match {
				case l : TLockable => try {
					if ( ! l._lock.isLocked ) { println("Attempting to unlock when no lock has yet been acquired") }
					else if ( ! l._lock.isHeldByCurrentThread ) { println("Attempting to unlock lock held by other thread") }
					l.unlock()
				} catch { case e => println("Exception while trying to unlock " + l);throw e }
			}
		}
	}

	def withFallback : TTaleaGridWindow[T,TaleaType] = new TaleaGridWindowWithFallback[T,TaleaType](this,grid,-Talea.dimension,Talea.dimension*2)
}

class TaleaGridWindowWithFallback[@specialized(Byte,Short,Int) T,TaleaType <: ITalea[T]] (window : TTaleaGridWindow[T,TaleaType], grid : TTaleaGrid[T,TaleaType], min : Int,max : Int) extends TTaleaGridWindow[T,TaleaType] {
	private final val cx = window.center.x
	private final val cy = window.center.y
	private final val cz = window.center.z
	private final val MIN = min
	private final val MAX = max
	/** Sets voxel to v, if v > current, returns current */
	override def setIfGreater(x: Int, y: Int, z: Int, v: T): T = if (x >= MIN && x < MAX && y >= MIN && y < MAX && z >= MIN && z < MAX) {
		window.setIfGreater(x,y,z,v)
	} else {
		grid.taleaFor(cx+x,cy+y,cz+z).setIfGreater(x,y,z,v)
	}
	override def unlock(): Unit = window.unlock()
	override def aboveCenterTalea: TaleaType = window.aboveCenterTalea
	override def belowCenterTalea: TaleaType = window.belowCenterTalea
	override def taleaContaining(x: Int, y: Int, z: Int): TaleaType = if (x >= MIN && x < MAX && y >= MIN && y < MAX && z >= MIN && z < MAX) {
		window.taleaContaining(x,y,z)
	} else {
		grid.taleaFor(cx+x,cy+y,cz+z)
	}
	override def center: ReadVec3i = window.center
	override def lock(): Unit = window.lock()
	override def taleae: Array[TaleaType] = window.taleae
	override def centerTalea: TaleaType = window.centerTalea
	override def setIfEqual(x: Int, y: Int, z: Int, curV: T, newV: T): T = if (x >= MIN && x < MAX && y >= MIN && y < MAX && z >= MIN && z < MAX) {
		window.setIfEqual(x,y,z,curV,newV)
	} else {
		grid.setIfEqual(cx+x,cy+y,cz+z,curV,newV)
	}
	override def setIfEqualRaw(x: Int, y: Int, z: Int, curV: T, newV: T): T = if (x >= MIN && x < MAX && y >= MIN && y < MAX && z >= MIN && z < MAX) {
		window.setIfEqualRaw(x,y,z,curV,newV)
	} else {
		grid.setIfEqual(cx+x,cy+y,cz+z,curV,newV)
	}
	override def update(x: Int, y: Int, z: Int, t: T): Unit = if (x >= MIN && x < MAX && y >= MIN && y < MAX && z >= MIN && z < MAX) {
		window.update(x,y,z,t)
	} else {
		grid.update(cx+x,cy+y,cz+z,t)
	}
	override def apply(x: Int, y: Int, z: Int): T = if (x >= MIN && x < MAX && y >= MIN && y < MAX && z >= MIN && z < MAX) {
		window.apply(x,y,z)
	} else {
		grid.apply(cx+x,cy+y,cz+z)
	}

	def withFallback : TTaleaGridWindow[T,TaleaType] = this
}

object GenericTaleaGridWindow {
	val offsets = Array.ofDim[Vec3i](64)

	def init () {
		var incrementor = 0
		var z = -1;
		while ( z <= 2 ) {
			var y = -1
			while ( y <= 2 ) {
				var x = -1
				while ( x <= 2 ) {
					val index = (x + 1) + ((y + 1) << 2) + ((z + 1) << 4)
					offsets(index) = Vec3i(x,y,z) * Talea.dimension
				x += 1}
			y += 1}
		z+=1}
	}
	init()
}

class ByteTaleaGridWindow[TaleaType <: GenericTalea[Byte] : Manifest] ( _grid : TTaleaGrid[Byte,TaleaType] , _center: VoxelCoord )
	extends GenericTaleaGridWindow[Byte,TaleaType](_grid,_center)
{
	def lerp ( vx : Float , vy : Float , vz : Float , a : Array[Float] ) : Float = {
		val ix = vx.toInt
		val iy = vy.toInt
		val iz = vz.toInt

		val fvx = vx - scala.math.floor(vx).toFloat
		val fvy = vy - scala.math.floor(vy).toFloat
		val fvz = vz - scala.math.floor(vz).toFloat

		val values = Array.ofDim[Byte](8)
		getBlock2x2(ix,iy,iz,values)
		val sum =
			a(values(0)) * (1.0f - fvx) * (1.0f - fvy) * (1.0f - fvz) +
			a(values(4)) * (1.0f - fvx) * (1.0f - fvy) * (fvz) +
			a(values(2)) * (1.0f - fvx) * (fvy) * (1.0f - fvz) +
			a(values(5)) * (1.0f - fvx) * (fvy) * (fvz) +
			a(values(1)) * (fvx) * (1.0f - fvy) * (1.0f - fvz) +
			a(values(6)) * (fvx) * (1.0f - fvy) * (fvz) +
			a(values(3)) * (fvx) * (fvy) * (1.0f - fvz) +
			a(values(7)) * (fvx) * (fvy) * (fvz)

		sum
	}
}

@SerialVersionUID(1L)
class ExtendedGenericTaleaGridWindow[@specialized(Byte,Short,Int) T,TaleaType <: ITalea[T] : Manifest] ( grid : TTaleaGrid[T,TaleaType] , val center: ReadVec3i , readOnly : Boolean = false ) extends
			TTaleaGridWindow[T,TaleaType] with Serializable
{


	protected val dimensionPo2 = Talea.dimensionPo2
	protected val dimensionm1 = Talea.dimension - 1
	protected val dimension = Talea.dimension
	protected val dimensionT2 = Talea.dimension * 2
	protected val dimensionT3 = Talea.dimension * 3
	protected val _rawTaleae = new Array[TaleaType](293) //we have fake dimensions of 4, so we can use shifts, but the maxium we can get is 2+2*4+2*16
	def rawTaleae( x : Int ,y : Int , z : Int ) : TaleaType = {
		Prelude.posit( x >= -dimensionT2 && x < dimensionT3 && y >= -dimensionT2 && y < dimensionT3 && z >= -dimensionT2 && z < dimensionT3 , "XYZ oob : " + x + "," + y + "," + z)
		rawTaleae( ((x >> dimensionPo2) + 2) + (((y >> dimensionPo2) + 2) << 3) + (((z >> dimensionPo2) + 2) << 6) )
	}
	def rawTaleae( index : Int ) : TaleaType = {
		if ( _rawTaleae(index) != null ) { _rawTaleae(index) }
		else {
			_rawTaleae(index) = grid.rawGetTalea(
				center.x + ExtendedGenericTaleaGridWindow.offsets(index).x,
				center.y + ExtendedGenericTaleaGridWindow.offsets(index).y,
				center.z + ExtendedGenericTaleaGridWindow.offsets(index).z,
				readOnly
			)
			_rawTaleae(index)
		}
	}
	var _taleae : Array[TaleaType] = null
	val centerIndex = (2) + ((2) << 3) + ((2) << 6)

	def initTaleae () {
		var incrementor = 0
		var z = -2;
		while ( z <= 2 ) {
			var y = -2
			while ( y <= 2 ) {
				var x = -2
				while ( x <= 2 ) {
					if ( scala.math.abs(x) + scala.math.abs(y) + scala.math.abs(z) <= 3 ) { //possible we could even reduce this to 2
						val index = (x + 2) + ((y + 2) << 3) + ((z + 2) << 6)
						_taleae(incrementor) = rawTaleae(index)
						incrementor += 1
					}
				x += 1}
			y += 1}
		z += 1}
	}

	def taleae : Array[TaleaType] = {
		if ( _taleae == null ) {
			_taleae = new Array[TaleaType](57) //Keep an easily iterable array in z/y/x ordering, could probably be reduced in size
			//assuming we go with the star pattern
			initTaleae()
		}
		_taleae
	}

	def centerTalea : TaleaType = { rawTaleae(centerIndex) }
	def aboveCenterTalea : TaleaType = { rawTaleae(2 + (2 << 3) + (3 << 6)) }
	def belowCenterTalea : TaleaType = { rawTaleae(2 + (2 << 3) + (1 << 6)) }
	def taleaContaining ( x : Int , y : Int , z : Int ) : TaleaType = { rawTaleae(x,y,z) }

	def relativeTo( v : ReadVec3i )( x : Int , y : Int , z : Int ) : T = { this((v.x - center.x) + x,(v.y - center.y) + y,(v.z - center.z) + z) }

	def apply ( x: Int, y: Int, z: Int ): T = {
		val t = rawTaleae(x,y,z)
		t((x + dimensionT2) & dimensionm1,(y + dimensionT2) & dimensionm1,(z + dimensionT2) & dimensionm1)
	}

	def update ( x: Int, y: Int, z: Int , b: T) {
		val t = rawTaleae(x,y,z)
		Prelude.posit( t != null , "Attempting to access invalid voxel in window : " + x + "," + y + "," + z )
		t((x + dimensionT2) & dimensionm1,(y + dimensionT2) & dimensionm1,(z + dimensionT2) & dimensionm1) = b
	}
	def setIfEqual(x: Int, y: Int, z: Int, curV: T, newV: T) : T = {
		val t = rawTaleae(x,y,z)
		t.setIfEqual((x + dimensionT2) & dimensionm1,(y + dimensionT2) & dimensionm1,(z + dimensionT2) & dimensionm1,curV,newV)
	}
	def setIfEqualRaw(x: Int, y: Int, z: Int, curV: T, newV: T) : T = {
		val t = rawTaleae(x,y,z)
		t.setIfEqualRaw((x + dimensionT2) & dimensionm1,(y + dimensionT2) & dimensionm1,(z + dimensionT2) & dimensionm1,curV,newV)
	}
	/** Equivalent to <code> window(x,y,z) = math.max(window(x,y,z),value)</code>
	  * @return the new value*/
	def setIfGreater(x: Int, y: Int,z : Int,newV: T) : T = {
		val t = rawTaleae(x,y,z)
		t.setIfGreater((x + dimensionT2) & dimensionm1,(y + dimensionT2) & dimensionm1,(z + dimensionT2) & dimensionm1,newV)
	}

	def lock () { taleae.foreach {
		case l: TLockable => l.lock ()
		case _ =>
	}}
	def unlock () { taleae.reverse.foreach {
		case l: TLockable =>
			try {
				l.unlock ()
			} catch {
				case e => {
					println ("Exception while attempting to unlock : " + l.toString)
					throw e
				}
			}
		case _ => 
	}}

	def withFallback : TTaleaGridWindow[T,TaleaType] = new TaleaGridWindowWithFallback(this,grid,-dimensionT2,dimensionT3)
}
object ExtendedGenericTaleaGridWindow {
	val offsets = Array.ofDim[Vec3i](293)

	def init () {
		var z = -2;
		while ( z <= 2 ) {
			var y = -2
			while ( y <= 2 ) {
				var x = -2
				while ( x <= 2 ) {
					val index = (x + 2) + ((y + 2) << 3) + ((z + 2) << 6)
					offsets(index) = Vec3i(x,y,z) * Talea.dimension
				x += 1}
			y += 1}
		z+=1}
	}
	init()
}