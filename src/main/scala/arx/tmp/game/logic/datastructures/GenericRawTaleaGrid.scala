package arx.tmp.game.logic.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/9/12
 * Time: 8:22 AM
 * Created by nonvirtualthunk
 */

import java.io.Externalizable
import java.io.ObjectInput
import java.io.ObjectOutput

import arx.core.vec.Vec3i
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.taleae.TaleaeModificationBlock
import arx.tmp.game.logic.world.SpatialRegion

import scala.collection.mutable
import scalaxy.loops._

class GenericRawTaleaGrid[@specialized(Byte,Short,Int) T,TaleaType <: ITalea[T]]( var defaultValue : T, newTaleaFunc : (VoxelCoord) => TaleaType)(implicit man : Manifest[TaleaType])
	extends TTaleaGrid[T,TaleaType] with Externalizable
{
	type MapType = RawGrid[TaleaType]

//	var newTaleaFunc = {
////		(pos:VoxelCoord) => null.asInstanceOf[TaleaType]
//		val ret = man.tpe.typeArgs.head match {
//			case b if b =:= typeOf[Byte] =>
//				val constructor = man.mirror.runtimeClass(man.tpe).getConstructor(classOf[VoxelCoord],classOf[Byte])
////				(v : VoxelCoord) => {
////					constructor.newInstance(v,Byte.box(defaultValue.asInstanceOf[Byte])).asInstanceOf[TaleaType]
////				}
////			case s : Short =>
////				val constructor = manifest[TaleaType].runtimeClass.getConstructor(classOf[VoxelCoord],classOf[Short])
////				(v : VoxelCoord) => {
////					constructor.newInstance(v,Short.box(s)).asInstanceOf[TaleaType]
////				}
////			case i : Int =>
////				val constructor = manifest[TaleaType].runtimeClass.getConstructor(classOf[VoxelCoord],classOf[Int])
////				(v : VoxelCoord) => {
////					constructor.newInstance(v,Int.box(i)).asInstanceOf[TaleaType]
////				}
////			case f : Float =>
////				val constructor = manifest[TaleaType].runtimeClass.getConstructor(classOf[VoxelCoord],classOf[Float])
////				(v : VoxelCoord) => {
////					constructor.newInstance(v,Float.box(f)).asInstanceOf[TaleaType]
////				}
//		}
//		ret
//	}


	var m_taleae : MapType = new RawGrid[TaleaType]( VoxelCoord.Center - Vec3i(1024,1024,512) , Vec3i(2048,2048,1024) )

	var storedManifest = man

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

	def createTalea ( pos : VoxelCoord ) : TaleaType = {
		val tt = newTaleaFunc(pos)
		tt.defaultValue = defaultValue
		tt.position = pos
		if ( isLoggingEnabled ) { tt.enableLogging() } else { tt.disableLogging() }
		tt
	}


	def taleaFor(x : Int,y: Int,z: Int) : TaleaType = {
		m_taleae.getOrElseUpdate( 	x,y,z , createTalea((VoxelCoord(x,y,z) >> Talea.dimensionPo2) << Talea.dimensionPo2) )
	}
	def taleaForReadOnly(x : Int,y: Int,z: Int) : TaleaType = {
		m_taleae.getOrElse( 	x,y,z , dummyTalea )
	}


	def apply(x: Int,y: Int,z: Int): T = {
		val t = taleaForReadOnly(x,y,z)
		val tx = t.x
		val ty = t.y
		val tz = t.z
		t(x - tx,y - ty,z - tz)
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
	def setUnsafe(x:Int,y:Int,z:Int,newV:T) {
		val t = taleaFor(x,y,z)
		t.setUnsafe(x - t.x,y - t.y,z - t.z,newV)
	}
	def getAndDecrementToMinimumOf(x:Int,y:Int,z:Int,minimumValue : T) : T = {
		val t = taleaFor(x,y,z)
		t.getAndDecrementToMinimumOf(x - t.x,y - t.y,z - t.z,minimumValue)
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
			ret(i) = getModifiedCount(v.x + Talea.cardinals(i).x,v.y + Talea.cardinals(i).y,v.z + Talea.cardinals(i).z)
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
		m_taleae.contains( x,y,z )
	}
	def definedInRegion(region: SpatialRegion): Boolean = {
		val start = region.lowerCorner
		val end = region.higherCorner

		val shiftedStart = start >> Talea.dimensionPo2
		val shiftedEnd = end >> Talea.dimensionPo2

		var x = shiftedStart.x;while ( x <= shiftedEnd.x ) {
			var y = shiftedStart.y;while ( y <= shiftedEnd.y ) {
				var z = shiftedStart.z;while ( z <= shiftedEnd.z ){
					val location = VoxelCoord(x << Talea.dimensionPo2,y << Talea.dimensionPo2,z << Talea.dimensionPo2)
					if ( definedAt(location.x,location.y,location.z) ) { return true }
				z+=1}
			y+=1}
		x+=1}
		false
	}

	def rawGetTalea (v: VoxelCoord, readOnly : Boolean): TaleaType = rawGetTalea(v.x,v.y,v.z)
	def rawGetTalea (x: Int,y: Int,z: Int, readOnly : Boolean): TaleaType = { if ( readOnly ) { rawGetTaleaRO(x,y,z) } else { rawGetTalea(x,y,z) } }
	def rawGetTalea (x: Int,y: Int,z: Int): TaleaType = {
//		val nx = x >> Talea.dimensionPo2
//		val ny = y >> Talea.dimensionPo2
//		val nz = z >> Talea.dimensionPo2
//		val h = Talea.hashPreShifted(nx,ny,nz)
		m_taleae.getOrElseUpdate( x,y,z , createTalea((VoxelCoord(x,y,z) >> Talea.dimensionPo2) << Talea.dimensionPo2) )
	}
	def rawGetTaleaRO (v : VoxelCoord): TaleaType = rawGetTaleaRO(v.x,v.y,v.z)
	def rawGetTaleaRO (x: Int,y: Int,z: Int): TaleaType = {
		m_taleae.getOrElse( x,y,z , dummyTalea )
	}

	def rawGetTaleaOrNull ( v : VoxelCoord ) : TaleaType = rawGetTaleaOrNull(v.x,v.y,v.z)
	def rawGetTaleaOrNull ( x : Int , y : Int , z : Int ) : TaleaType = {
		m_taleae.getOrElse( x,y,z , null.asInstanceOf[TaleaType] )
	}
	def rawGetTaleaIfExists (v : VoxelCoord) : Option[TaleaType] = { rawGetTaleaIfExists(v.x,v.y,v.z) }
	def rawGetTaleaIfExists (x : Int,y: Int,z : Int) : Option[TaleaType] = {
		m_taleae.get( x,y,z )
	}

	def setTalea (talea : TaleaType){
		m_taleae.put( talea.x,talea.y,talea.z,talea)
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

		for ( x <- shiftedStart.x to shiftedEnd.x optimized; y <- shiftedStart.y to shiftedEnd.y optimized; z <- shiftedStart.z to shiftedEnd.z optimized){
			val location = VoxelCoord(x << Talea.dimensionPo2,y << Talea.dimensionPo2,z << Talea.dimensionPo2)
			ret ::= rawGetTalea(location.x,location.y,location.z)
		}
		ret
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
			this.asInstanceOf[TTaleaGrid[Byte,BType]],
			(center >> Talea.dimensionPo2) << Talea.dimensionPo2
		)(manifest[BType])
	}

	def windowForRegion ( region: SpatialRegion ) : GenericTaleaGridRegionWindow[T,TaleaType] = {
		new GenericTaleaGridRegionWindow(this,region)(storedManifest)
	}

	def writeExternal(p1: ObjectOutput) {
//		p1.writeNBHMi(m_taleae)
		p1.writeObject(m_taleae)
//		p1.writeObject(newTaleaFunc)
//		p1.writeObject(storedManifest)
		p1.writeObject(userData)
		p1.writeObject(dummyTalea)
		p1.writeObject(activeTalea)
	}

	def readExternal(p1: ObjectInput) {
//		m_taleae = p1.readNBHMi[Int,TaleaType]
		m_taleae = p1.readAs[MapType]
//		newTaleaFunc = p1.readAs[(VoxelCoord) => TaleaType]
//		storedManifest = p1.readAs[Manifest[TaleaType]]
		userData = p1.readAs[Option[Any]]
		dummyTalea = p1.readAs[TaleaType]
		activeTalea = p1.readAs[TaleaType]
	}

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

	def view ( offset : VoxelCoord ) = new GenericRawTaleaGridView[T](this,offset)
}