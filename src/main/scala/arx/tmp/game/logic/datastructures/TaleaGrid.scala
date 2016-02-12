package arx.tmp.game.logic.datastructures

import java.io._

import arx.core.datastructures.AtomicMapi
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec3i
import arx.tmp.game.logic.Coord
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.voxelregions.VoxelRegion
import arx.tmp.game.logic.taleae.TaleaeModificationBlock
import arx.tmp.game.logic.world.SpatialRegion

import scala.collection.mutable

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 6/10/11
 * Time: 9:02 AM 
 */


class TaleaGrid extends GenericTaleaGrid[Byte,ByteTalea]( 0.toByte, (v:VoxelCoord) => new ByteTalea(v,0.toByte)) {}

/**
 * An infinite, voxel grid. Lazily initializes chunks as they become necessary, backed internally by a non blocking hash map. Access
 * checks against a thread local last-accessed talea, then against the overall map. Implements TEventUser but does not, by default
 * fire any events, nor does it serialize connections and listener relationships. Can produce windows, views onto specific sub-regions
 * of the voxel field for more efficient access. These usually take the form of either set 3x3x3 windows around a given talea, but
 * can also be made for a generic three dimensional spatial region.
 */
class GenericTaleaGrid[@specialized(Byte,Short,Int) T, TaleaType <: ITalea[T] : Manifest]( var defaultValue : T , newTaleaFunc : (VoxelCoord) => TaleaType )
	extends TTaleaGrid[T,TaleaType] with Externalizable //with KryoSerializable
{
	type MapType = AtomicMapi[Int,TaleaType]//AtomicMap[Int,TaleaType]

	var m_taleae : MapType = new AtomicMapi[Int,TaleaType]//AtomicMap.atomicNBHM[Int,TaleaType]


	var loggingEnabledCount = 0
	def enableLogging () { loggingEnabledCount += 1; if ( loggingEnabledCount == 1 ) { changeLogging(enable = true) } }
	def disableLogging () { loggingEnabledCount -= 1; if ( loggingEnabledCount == 0 ) { changeLogging(enable = false) } }
	def isLoggingEnabled : Boolean = loggingEnabledCount > 0
	def changeLogging (enable : Boolean) {
		for ( t <- m_taleae.values ) {
			if ( enable ) { t.enableLogging() } else { t.disableLogging() }
		}
	}

	def clear (b : T) {
		for ( t <- m_taleae.values ) {
			t.setAll(b)
		}
	}

	var dummyTalea = if ( newTaleaFunc != null ) { createTalea(VoxelCoord(-1,-1,-1)) } else { null.asInstanceOf[TaleaType] }
	var activeTalea : TaleaType = dummyTalea
	var activeReadOnlyTalea : TaleaType = dummyTalea
	var storedManifest = manifest[TaleaType]

	def createTalea ( pos : VoxelCoord ) : TaleaType = {
		val tt = newTaleaFunc(pos)
		tt.defaultValue = defaultValue
		tt.position = pos
		if ( isLoggingEnabled ) { tt.enableLogging() } else { tt.disableLogging() }
		tt
	}


	def taleaFor(x : Int,y: Int,z: Int) : TaleaType = {
		val cur = activeTalea
		val tDimPo2 = Talea.dimensionPo2
		val nx = x >> tDimPo2
		val ny = y >> tDimPo2
		val nz = z >> tDimPo2
		val shiftedPos = cur.shiftedPosition
		if ( ((nx) != (shiftedPos.x)) ||
			  ((ny) != (shiftedPos.y)) ||
			  ((nz) != (shiftedPos.z)) )
		{
			val newT = m_taleae.getOrElseUpdatei( 	Talea.hashPreShifted(nx,ny,nz) ,
																createTalea(VoxelCoord(nx << Talea.dimensionPo2,ny << Talea.dimensionPo2,nz << Talea.dimensionPo2)) )
			activeTalea = newT
			newT
		} else {
			cur
		}
	}
	def taleaForReadOnly(x : Int,y: Int,z: Int) : TaleaType = {
			val cur = activeReadOnlyTalea
			val tDimPo2 = Talea.dimensionPo2
			val nx = x >> tDimPo2
			val ny = y >> tDimPo2
			val nz = z >> tDimPo2
			val shiftedPos = cur.shiftedPosition
			if ( ((nx) != (shiftedPos.x)) ||
				  ((ny) != (shiftedPos.y)) ||
				  ((nz) != (shiftedPos.z)) )
			{
				val newT = m_taleae.getOrElsei( 	Talea.hashPreShifted(nx,ny,nz) ,
															dummyTalea )
				activeReadOnlyTalea = newT
				newT
			} else {
				cur
			}
		}


	def apply(x: Int,y: Int,z: Int): T = {
		val t = taleaForReadOnly(x,y,z)
		val taleaLoc = t.position
		t(x - taleaLoc.x,y - taleaLoc.y,z - taleaLoc.z)
	}
	def update(x: Int,y: Int,z: Int,b: T){
		val t = taleaFor(x,y,z)
		t(x - t.x,y - t.y,z - t.z) = b
	}
	def setIfEqual(x : Int,y: Int,z : Int,curV : T,newV : T) : T = {
		val t = taleaFor(x,y,z)
		t.setIfEqual(x - t.x,y - t.y,z - t.z,curV,newV)
	}
	def setIfNotEqual(x:Int,y:Int,z:Int,curV : T,newV : T) : T = {
		val t = taleaFor(x,y,z)
		t.setIfNotEqual(x - t.x,y - t.y,z - t.z,curV,newV)
	}

	def relativeTo( talea : ITalea[T] )(x : Int,y : Int,z : Int) : T = {
		this(talea.x + x,talea.y + y,talea.z + z)
	}
	def relativeTo( v : VoxelCoord )(x : Int,y : Int,z : Int) : T = {
		this(v.x + x,v.y + y,v.z + z)
	}
	def setRelativeTo( talea : ITalea[T] )(x : Int,y : Int,z : Int,b : T) {
		this(talea.x + x,talea.y + y,talea.z + z) = b
	}

	def getModifiedCount(x : Int , y : Int , z : Int): Int = {
		val t = rawGetTaleaOrNull(x,y,z)
		if ( t == null ) { 0 }
		else { t.modifiedCount }
	}
	def getModifiedCount(v: VoxelCoord): Int = {
		getModifiedCount(v.x,v.y,v.z)
	}

	def getModifiedCountIncludingAdjacents(v: VoxelCoord): Array[Long] = {
		val ret = Array.ofDim[Long](7)
		var i = 0; while ( i < 7 ) {
			ret(i) = getModifiedCount(v + Talea.cardinals(i))
		i += 1}

		ret
	}
	def getModifiedCountSumIncludingAdjacents(v : VoxelCoord) : Long = {
		var ret = 0
		var i = 0; while ( i < 7 ) {
			ret += getModifiedCount(v.x + Talea.cardinals(i).x,v.y + Talea.cardinals(i).y,v.z + Talea.cardinals(i).z)
		i += 1}
		ret
	}

	def definedAt ( v : VoxelCoord ) : Boolean = definedAt(v.x,v.y,v.z)
	def definedAt ( x : Int , y : Int , z : Int ) : Boolean = {
		val nx = x >> Talea.dimensionPo2
		val ny = y >> Talea.dimensionPo2
		val nz = z >> Talea.dimensionPo2
		m_taleae.contains( Talea.hashPreShifted(nx,ny,nz) )
	}
	def definedInRegion(region: SpatialRegion): Boolean = {
		val start = region.lowerCorner
		val end = region.higherCorner

		val shiftedStart = start >> Talea.dimensionPo2
		val shiftedEnd = end >> Talea.dimensionPo2

		var x = shiftedStart.x;while ( x <= shiftedEnd.x ) {
			var y = shiftedStart.y;while ( y <= shiftedEnd.y ) {
				var z = shiftedStart.z;while ( z <= shiftedEnd.z ){
					val location = Vec3i(x << Talea.dimensionPo2,y << Talea.dimensionPo2,z << Talea.dimensionPo2)
					if ( definedAt(location.x,location.y,location.z) ) { return true }
				z+=1}
			y+=1}
		x+=1}
		false
	}

	def rawGetTalea (v: VoxelCoord, readOnly : Boolean): TaleaType = rawGetTalea(v.x,v.y,v.z)
	def rawGetTalea (x: Int,y: Int,z: Int, readOnly : Boolean): TaleaType = { if ( readOnly ) { rawGetTaleaRO(x,y,z) } else { rawGetTalea(x,y,z) } }
	def rawGetTalea (x: Int,y: Int,z: Int): TaleaType = {
		val nx = x >> Talea.dimensionPo2
		val ny = y >> Talea.dimensionPo2
		val nz = z >> Talea.dimensionPo2
		val h = Talea.hashPreShifted(nx,ny,nz)
		m_taleae.getOrElseUpdatei( h , createTalea(VoxelCoord(nx << Talea.dimensionPo2,ny << Talea.dimensionPo2,nz << Talea.dimensionPo2)) )
	}
	def rawGetTaleaRO (v : VoxelCoord): TaleaType = rawGetTaleaRO(v.x,v.y,v.z)
	def rawGetTaleaRO (x: Int,y: Int,z: Int): TaleaType = {
		m_taleae.getOrElsei( Talea.hash(x,y,z) , dummyTalea )
	}

	def rawGetTaleaOrNull ( v : VoxelCoord ) : TaleaType = rawGetTaleaOrNull(v.x,v.y,v.z)
	def rawGetTaleaOrNull ( x : Int , y : Int , z : Int ) : TaleaType = {
		val nx = x >> Talea.dimensionPo2
		val ny = y >> Talea.dimensionPo2
		val nz = z >> Talea.dimensionPo2
		m_taleae.getOrElsei( Talea.hashPreShifted(nx,ny,nz) , null.asInstanceOf[TaleaType] )
	}
	def rawGetTaleaIfExists (v : VoxelCoord) : Option[TaleaType] = { rawGetTaleaIfExists(v.x,v.y,v.z) }
	def rawGetTaleaIfExists (x : Int,y: Int,z : Int) : Option[TaleaType] = {
		val nx = x >> Talea.dimensionPo2
		val ny = y >> Talea.dimensionPo2
		val nz = z >> Talea.dimensionPo2
		m_taleae.get( Talea.hashPreShifted(nx,ny,nz) )
	}

	def setTalea (talea : TaleaType){
		m_taleae.put(talea.hash,talea)
	}

	/**
	 * Returns all taleae that have at least one voxel in the specified region
	 */
	def allInRegionInclusiveSet ( region : SpatialRegion ): Set[TaleaType] = {
		allInRegionInclusive(region).toSet
	}

	def allInRegionInclusive ( region : SpatialRegion ) : List[TaleaType] = {
		var ret = List[TaleaType]()
		val start = region.lowerCorner
		val end = region.higherCorner

		val shiftedStart = start >> Talea.dimensionPo2
		val shiftedEnd = end >> Talea.dimensionPo2

		for ( x <- shiftedStart.x to shiftedEnd.x; y <- shiftedStart.y to shiftedEnd.y ; z <- shiftedStart.z to shiftedEnd.z ){
			val location = Vec3i(x << Talea.dimensionPo2,y << Talea.dimensionPo2,z << Talea.dimensionPo2)
			ret ::= rawGetTalea(location.x,location.y,location.z)
		}
		ret
	}

	def allTaleaPositionsInRegionInclusive ( region : VoxelRegion ) : List[ReadVec3i] = {
		val bounding = region.boundingRegion
		allTaleaPositionsInRegionInclusive(SpatialRegion.fromCorners(bounding.min,bounding.max))
	}
	def allTaleaPositionsInRegionInclusive ( region : SpatialRegion ) : List[VoxelCoord] = {
		var ret = List[VoxelCoord]()
		val start = region.lowerCorner
		val end = region.higherCorner

		val shiftedStart = start >> Talea.dimensionPo2
		val shiftedEnd = end >> Talea.dimensionPo2

		for ( x <- shiftedStart.x to shiftedEnd.x; y <- shiftedStart.y to shiftedEnd.y ; z <- shiftedEnd.z to shiftedStart.z by -1  ){
			val location = VoxelCoord(x << Talea.dimensionPo2,y << Talea.dimensionPo2,z << Talea.dimensionPo2)
			ret ::= location
		}
		ret
	}

	def windowCenteredOnTaleaAt ( center: VoxelCoord , readOnly : Boolean ) : GenericTaleaGridWindow[T,TaleaType] = {
		new GenericTaleaGridWindow(this,(center >> Talea.dimensionPo2) << Talea.dimensionPo2,readOnly)(storedManifest)
	}
	def extendedWindowCenteredOnTaleaAt ( center: VoxelCoord , readOnly : Boolean ) : ExtendedGenericTaleaGridWindow[T,TaleaType] = {
		new ExtendedGenericTaleaGridWindow(this,(center >> Talea.dimensionPo2) << Talea.dimensionPo2,readOnly)(storedManifest)
	}
	def byteWindowCenteredOnTaleaAt[BType <: GenericTalea[Byte] : Manifest] ( center : VoxelCoord ) : ByteTaleaGridWindow[BType] = {
		new ByteTaleaGridWindow[BType](
			this.asInstanceOf[GenericTaleaGrid[Byte,BType]],
			(center >> Talea.dimensionPo2) << Talea.dimensionPo2
		)(manifest[BType])
	}

	def windowForRegion ( region: SpatialRegion ) : GenericTaleaGridRegionWindow[T,TaleaType] = {
		new GenericTaleaGridRegionWindow(this,region)(storedManifest)
	}

//	def writeExternal(p1: ObjectOutput) {
//		p1.writeNBHMi(m_taleae)
////		p1.writeObject(m_taleae)
////		p1.writeObject(newTaleaFunc)
//		p1.writeObject(storedManifest)
//		p1.writeObject(userData)
//		p1.writeObject(dummyTalea)
//		p1.writeObject(activeTalea)
//	}
//
//	def readExternal(p1: ObjectInput) {
//		m_taleae = p1.readNBHMi[Int,TaleaType]
////		m_taleae = p1.readAs[MapType]
////		newTaleaFunc = p1.readAs[(VoxelCoord) => TaleaType]
//		storedManifest = p1.readAs[Manifest[TaleaType]]
//		userData = p1.readAs[Option[Any]]
//		dummyTalea = p1.readAs[TaleaType]
//		activeTalea = p1.readAs[TaleaType]
//		activeReadOnlyTalea = dummyTalea
//	}

//	override def write(kryo: Kryo, output: Output): Unit ={
//		kryo.writeClassAndObject(output,storedManifest)
//		val taleae = m_taleae.values
//		output.writeInt(taleae.size)
//		taleae.foreach(t => kryo.writeObject(output,t))
//
//	}
//
//	override def read(kryo: Kryo, input: Input): Unit ={
//		storedManifest = kryo.readClassAndObject(input).asInstanceOf[Manifest[TaleaType]]
//		val nTaleae = input.readInt()
//		for (i <- 0 until nTaleae) {
//			val talea = kryo.readObject[TaleaType](input,storedManifest.runtimeClass.asInstanceOf[Class[TaleaType]])
//			val nx = talea.x >> Talea.dimensionPo2
//			val ny = talea.y >> Talea.dimensionPo2
//			val nz = talea.z >> Talea.dimensionPo2
//			val h = Talea.hashPreShifted(nx,ny,nz)
//			m_taleae.update(h , talea)
//		}
//	}

	def getSubContainer(x: Int, y: Int, z: Int) : TaleaType = {
		rawGetTalea(x,y,z)
	}

	def subContainerDimension : Int = Talea.dimension
	def subContainerDimensionPo2 : Int = Talea.dimensionPo2

	def modificationBlock ( taleae : Iterable[TaleaType] )( stmt : => Unit ) {
		TaleaeModificationBlock(this,taleae)(stmt)
	}
	def modificationBlock ( window : GenericTaleaGridWindow[T,TaleaType] )( stmt : => Unit ) { modificationBlock(window.taleae)(stmt) }
	def modificationBlock ( region : SpatialRegion )( stmt : => Unit ) {
		modificationBlock( allInRegionInclusive(SpatialRegion.fromCorners(region.lowerCorner - 1,region.upperCorner + 1)) )(stmt)
	}
	def modificationBlock(pos: VoxelCoord)( stmt : => Unit ) {
		modificationBlock( allInRegionInclusive(SpatialRegion(pos,Vec3i(3,3,3))) )(stmt)
	}
	def modificationBlock(pos: List[VoxelCoord])( stmt : => Unit ) {
		modificationBlock( allInRegionInclusive(SpatialRegion.fromPoints(pos,1)) )(stmt)
	}
	def modificationBlock(pos: Set[VoxelCoord])( stmt : => Unit ) {
		modificationBlock( allInRegionInclusive(SpatialRegion.fromPoints(pos,1)) )(stmt)
	}
	def modificationBlock(pos: mutable.HashSet[VoxelCoord])( stmt : => Unit ) {
		modificationBlock( allInRegionInclusive(SpatialRegion.fromPoints(pos,1)) )(stmt)
	}
	def modificationBlock(pos: VoxelRegion)( stmt : => Unit ) {
		modificationBlock( allInRegionInclusive(SpatialRegion.fromPoints(pos,1)) )(stmt)
	}
	def deferredModificationBlock( taleae : Iterable[TaleaType] )( stmt : => Unit ) : Set[ITalea[_]] = {
		TaleaeModificationBlock(this,taleae,fire = false)(stmt)
	}
	def deferredModificationBlock( window : GenericTaleaGridWindow[T,TaleaType] )( stmt : => Unit ) : Set[ITalea[_]] = {
		TaleaeModificationBlock(this,window.taleae,fire = false)(stmt)
	}
	def deferredModificationBlock( window : ExtendedGenericTaleaGridWindow[T,TaleaType] )( stmt : => Unit ) : Set[ITalea[_]] = {
		TaleaeModificationBlock(this,window.taleae,fire = false)(stmt)
	}
	def setAndNotify(pos: VoxelCoord,v : T) {
		modificationBlock( allInRegionInclusive(SpatialRegion(pos,Vec3i(3,3,3))) ) {
			val t = taleaFor(pos.x,pos.y,pos.z)
			t(pos.x - t.x,pos.y - t.y,pos.z - t.z) = v
		}
	}

	def allTaleae = m_taleae.values
}



class TaleaGridRegionWindow(_grid: TaleaGrid,_region: SpatialRegion) extends GenericTaleaGridRegionWindow[Byte,ByteTalea](_grid,_region){}
/**
 * In the window's coordinates, (0,0,0) is the bottom left of the region
 */
@SerialVersionUID(1L)
class GenericTaleaGridRegionWindow[@specialized(Byte,Short,Int) T, TaleaType <: ITalea[T] : Manifest]( val grid: TTaleaGrid[T,TaleaType] , val region: SpatialRegion ) extends Serializable{
	val basis = region.lowerCorner
	val effectiveRegion = new SpatialRegion(region.center,region.dimensions + Talea.dimension * 2)
	val tDimensions = (Coord.w2t(effectiveRegion.higherCorner) + 1) - Coord.w2t(effectiveRegion.lowerCorner)
	val taleae = new Array[TaleaType]( tDimensions.x * tDimensions.y * tDimensions.z )
	def index (x: Int,y: Int,z: Int): Int = { x + (tDimensions.x * y) + (tDimensions.x * tDimensions.y * z) }

	val shiftedStart = Coord.w2t(effectiveRegion.lowerCorner)
	val shiftedEnd = Coord.w2t(effectiveRegion.higherCorner)
	for ( x <- shiftedStart.x to shiftedEnd.x ; y <- shiftedStart.y to shiftedEnd.y ; z <- shiftedStart.z to shiftedEnd.z ){
		taleae( index(x - shiftedStart.x,y - shiftedStart.y,z - shiftedStart.z) ) = grid.rawGetTalea( Coord.t2w_v(VoxelCoord(x,y,z)) , readOnly = false )
	}

	def taleaContaining (rx: Int,ry: Int,rz: Int): TaleaType = {
		val wx = (rx + basis.x)
		val wy = (ry + basis.y)
		val wz = (rz + basis.z)
		val x = (wx >> Talea.dimensionPo2) - shiftedStart.x
		val y = (wy >> Talea.dimensionPo2) - shiftedStart.y
		val z = (wz >> Talea.dimensionPo2) - shiftedStart.z

		taleae( index(x,y,z) )
	}

	def apply (rx: Int,ry: Int,rz: Int): T = {
		val wx = (rx + basis.x)
		val wy = (ry + basis.y)
		val wz = (rz + basis.z)
		val x = (wx >> Talea.dimensionPo2) - shiftedStart.x
		val y = (wy >> Talea.dimensionPo2) - shiftedStart.y
		val z = (wz >> Talea.dimensionPo2) - shiftedStart.z

		val talea = taleae( index(x,y,z) )
		talea(wx - talea.x,wy - talea.y,wz - talea.z)
	}

	def update (rx: Int,ry: Int,rz: Int, b: T) {
		val wx = (rx + basis.x)
		val wy = (ry + basis.y)
		val wz = (rz + basis.z)
		val x = (wx >> Talea.dimensionPo2) - shiftedStart.x
		val y = (wy >> Talea.dimensionPo2) - shiftedStart.y
		val z = (wz >> Talea.dimensionPo2) - shiftedStart.z

		val talea = taleae( index(x,y,z) )
		talea(wx - talea.x,wy - talea.y,wz - talea.z) = b
	}


}

object TaleaGrid {
	def allTaleaRegionsInRegion (region: SpatialRegion): List[SpatialRegion] = {
		var ret = List[SpatialRegion]()
		val start = region.lowerCorner
		val end = region.higherCorner

		val shiftedStart = start >> Talea.dimensionPo2
		val shiftedEnd = end >> Talea.dimensionPo2

		for ( x <- shiftedStart.x to shiftedEnd.x; y <- shiftedStart.y to shiftedEnd.y ; z <- shiftedStart.z to shiftedEnd.z ){
			val location = Vec3i(x << Talea.dimensionPo2,y << Talea.dimensionPo2,z << Talea.dimensionPo2)
			val taleaRegion = SpatialRegion.fromCorners(location,location + Talea.dimension)
			ret ::= region.intersection(taleaRegion)
		}
		ret
	}

	case class TaleaModificationsCompletedEvent ( talea : Iterable[ITalea[_]] ) extends Event {
		var preModificationCounts = Map[ITalea[_],Long]()
	}
}