package arx.tmp.game.logic.world.data

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/6/12
 * Time: 12:22 PM
 * Created by nonvirtualthunk
 */

import java.io.Externalizable
import java.io.ObjectInput
import java.io.ObjectOutput

import arx.core.Moddable
import arx.core.vec.Vec3i
import arx.core.vec.Vec3f
import arx.core.vec.Vec4f
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.ByteTalea
import arx.tmp.game.logic.datastructures.GenericRawTaleaGrid
import arx.tmp.game.logic.datastructures.GenericTaleaGrid
import arx.tmp.game.logic.datastructures.GenericTaleaGridWindow
import arx.tmp.game.logic.entities.TLightSource
import arx.tmp.game.logic.world.SpatialRegion

import scala.collection.mutable.ArrayBuffer

/**
 */
class LightData extends TWorldAuxData with TEventUser with Externalizable {
	protected var _FullLight = 32.toByte
	def FullLight = _FullLight
	def FullLight_= (f : Byte) { _FullLight = f ; updateMults() }

	var globalLighting = new ArrayBuffer[GenericRawTaleaGrid[Byte,LightData.LightTaleaType]]()
	for (i <- 0 until 3) {
		globalLighting.append( new GenericRawTaleaGrid[Byte,LightData.LightTaleaType](FullLight,(v:VoxelCoord) => new LightData.LightTaleaType (v,FullLight )) ) //Global lighting
	}
	globalLighting.foreach( _.onEvent { case e : Event => this.fireEvent(e) }  )

	var lowSkyColor = Vec3f(1.0f,1.0f,1.0f)
	var highSkyColor = Vec3f(0.61f,0.87f,1.0f)

	var globalLightStrength = Array(1.0f)
	var globalLightColor = Array(Vec3f(1.0f,1.0f,1.0f))

	protected var localLighting = new SpaceConservingArray[LightData.LocalLightChannel]()

	protected var _lightMults = Array.ofDim[Float](FullLight * 2 + 1)
	protected var _localLightMults = Array.ofDim[Float]((_lightMults.length - 1) * 2)
	updateMults()


	def localLightChannelFor ( lightSource : TLightSource ) = localLighting(lightSource.lightChannelIndex)
	def localLightChannel ( channelIndex : Int ) = localLighting(channelIndex)

	def addLocalLightChannel( grid : GenericTaleaGrid[Byte,LightData.LightTaleaType], color : Moddable[Vec4f] ) = {
		localLighting.put(new LightData.LocalLightChannel(grid,color,Nil))
	}
	def removeLocalLightChannel( channelIndex : Int ) {
		localLighting.removeAt(channelIndex)
	}

	def activeLocalLighting = localLighting.denseArraySnapshot

	def localLightingDefinedAt ( wloc : Vec3i ) : Array[LightData.LocalLightChannelWindow] = {
		val localLights = this.localLighting.denseArraySnapshot.filter( ll => ll.grid.definedAt ( wloc.x, wloc.y, wloc.z ) ).toArray
		val localLightWindows = localLights.map( ll => new LightData.LocalLightChannelWindow(ll.grid.windowCenteredOnTaleaAt( VoxelCoord(wloc) , readOnly = true ), ll.color) ).toArray
		localLightWindows
	}
	def localLightingDefinedIn ( region : SpatialRegion ) : Array[LightData.LocalLightChannelWindow] = {
		val localLights = this.localLighting.denseArraySnapshot.filter( _.grid.definedInRegion(region) )
		val localLightWindows = localLights.map( ll => new LightData.LocalLightChannelWindow(ll.grid.windowCenteredOnTaleaAt( VoxelCoord(region.center) , readOnly = true ),ll.color) ).toArray
		localLightWindows
	}
	def allLocalLightChannels : Array[LightData.LocalLightChannel] = {
		this.localLighting.denseArraySnapshot
	}

	def lightGridForLightSource(source: TLightSource) = { localLighting(source.lightChannelIndex).grid }

	def maximumLocalLightAt ( x : Int , y : Int, z : Int ) = {
		var maximum = 0
		var i = 0; while ( i < localLighting.occupiedIndices.length ) {
			val li = localLighting.occupiedIndices(i)
			maximum = math.max(maximum,localLighting(li).grid(x,y,z))
		i += 1}
		maximum
	}

	def lightRevisionAt ( loc : VoxelCoord ) = {
		var sum = localLightingRevisionAt(loc)
		val globalLightModifications = globalLighting(0).getModifiedCountIncludingAdjacents(loc)
		var k = 0; while ( k < globalLightModifications.length ) {
			sum += globalLightModifications(k)
		k += 1}
		sum
	}

	def localLightingRevisionAt ( loc : VoxelCoord ) = {
		var counter = 0L
		var xli = 0;while ( xli < this.localLighting.occupiedIndices.length ) {
			val li = this.localLighting.occupiedIndices(xli)
			val modificationCounts = this.localLighting(li).grid.getModifiedCountIncludingAdjacents(loc)
			var k = 0; while ( k < modificationCounts.length ) {
				counter += modificationCounts(k)
			k += 1}
		xli += 1}
		counter
	}

	@inline def globalLightByteToPcnt (b : Byte) = {
		_lightMults(math.max(b,0.toByte))
	}
	@inline def localLightByteToPcnt (b : Byte) = {
		_localLightMults(math.max(b,0.toByte))
	}
	
	def updateMults() {
		_lightMults = Array.ofDim[Float](FullLight * 2 + 1)
		for ( i <- 0 to FullLight * 2 ) {
			val log = scala.math.pow(0.93f,FullLight - i).toFloat
			val lin = (i).toFloat / FullLight
			
			_lightMults(i) = math.min(log * 0.6f + lin * 0.4f,1.9f)
		}

		_localLightMults = Array.ofDim[Float]((_lightMults.length - 1) * 2)
		for ( i <- 0 until (_lightMults.length-1) * 2 ) {
			if ( i % 2 == 0 ) {
				_localLightMults(i) = _lightMults(i>>1) - _lightMults(0)
			} else {
				_localLightMults(i) = (_lightMults(i>>1) + _lightMults((i>>1)+1)) * 0.5f - _lightMults(0)
			}
		}
	}

	def writeExternal(p1: ObjectOutput) {
		p1.writeByte(FullLight)
//		p1.writeObject(globalLighting)
//		p1.writeObject(lowSkyColor)
//		p1.writeObject(highSkyColor)
//		p1.writeObject(localLighting)
//		p1.writeObject(localLightColors)
	}
	def readExternal(p1: ObjectInput) {
		FullLight = p1.readByte()
//		globalLighting = p1.readAs[ArrayBuffer[GenericTaleaGrid[Byte,LightData.LightTaleaType]]]
//		lowSkyColor = p1.readAs[Vec4f]
//		highSkyColor = p1.readAs[Vec4f]
//		localLighting = p1.readAs[SpaceConservingArray[GenericTaleaGrid[Byte,LightData.LightTaleaType]]]
//		localLightColors = p1.readAs[SpaceConservingArray[Vec4f]]
//		globalLighting.foreach( _.onEvent { case e : Event => this.fireEvent(e) }  )
	}
}
object LightData {
	class LightTaleaType(pos : VoxelCoord,defaultValue : Byte) extends ByteTalea(pos,defaultValue) with Markable with TLockable

	class LocalLightChannel(val grid : GenericTaleaGrid[Byte,LightData.LightTaleaType], val color : Moddable[Vec4f] , var lightSources : List[TLightSource] ) {}
	class LocalLightChannelWindow(val window : GenericTaleaGridWindow[Byte,LightData.LightTaleaType], val color : Vec4f){}
}

object FullDarknessLightData extends LightData {
//	Noto.warn("Where the fuck is this coming from?")
}
object FullLightLightData extends LightData {
	globalLighting(0) = new GenericRawTaleaGrid[Byte,LightData.LightTaleaType](FullLight,(v:VoxelCoord) => new LightData.LightTaleaType (v,FullLight ))
}