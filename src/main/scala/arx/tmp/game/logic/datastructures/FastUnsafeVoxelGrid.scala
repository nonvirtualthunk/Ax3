package arx.tmp.game.logic.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/11/12
 * Time: 12:34 PM
 * Created by nonvirtualthunk
 */

import java.io.ObjectInput
import java.io.ObjectOutput

import arx.core.vec.Vec3i
import arx.core.vec.coordinates.VoxelCoord

class FastUnsafeVoxelGrid(center:VoxelCoord,var powerOfTwoDimensions:Vec3i,var defaultValue:Byte) extends TInfiniteVoxelStore[Byte] {
	def this() { this(VoxelCoord(0,0,0),Vec3i(1,1,1),0.toByte) }
	var coreDimensions = Vec3i(1 << powerOfTwoDimensions.x,1 << powerOfTwoDimensions.y,1 << powerOfTwoDimensions.z)
	var coreShiftedPo2 = Vec3i(powerOfTwoDimensions.x - Talea.dimensionPo2,powerOfTwoDimensions.y - Talea.dimensionPo2,powerOfTwoDimensions.z - Talea.dimensionPo2)
	var yshift = coreShiftedPo2.x
	var zshift = coreShiftedPo2.x + coreShiftedPo2.y

	var basePoint = center - (coreDimensions/2)
	var farPoint = center + (coreDimensions/2)
	var coreArray = Array.ofDim[ByteTalea]( 	(coreDimensions.x >> Talea.dimensionPo2) *
															(coreDimensions.y >> Talea.dimensionPo2) *
															(coreDimensions.z >> Talea.dimensionPo2 ) )
	var fallbackGrid = new GenericTaleaGrid[Byte,ByteTalea](defaultValue,(v:VoxelCoord) => new ByteTalea (v,defaultValue ))


	def apply(x: Int, y: Int, z: Int) = {
		val rx = x - basePoint.x
		val ry = y - basePoint.y
		val rz = z - basePoint.z
		if ( rx >= 0 && rx < coreDimensions.x && ry >= 0 && ry < coreDimensions.y && rz >= 0 && rz < coreDimensions.z ) {
			val taleaIndex = (rx >> Talea.dimensionPo2) + ((ry>> Talea.dimensionPo2) << yshift) + ((rz>> Talea.dimensionPo2) << zshift)
			var talea = coreArray(taleaIndex)
			if ( talea == null ) {
				talea = new ByteTalea(VoxelCoord((Vec3i(x,y,z) >> Talea.dimensionPo2) << Talea.dimensionPo2),defaultValue)
				coreArray(taleaIndex) = talea
			}
			talea( x - talea.x , y - talea.y , z - talea.z )
		} else {
			fallbackGrid(x,y,z)
		}
	}
	def update(x: Int, y: Int, z: Int, t: Byte) {
		val rx = x - basePoint.x
		val ry = y - basePoint.y
		val rz = z - basePoint.z
		if ( rx >= 0 && rx < coreDimensions.x && ry >= 0 && ry < coreDimensions.y && rz >= 0 && rz < coreDimensions.z ) {
			val taleaIndex = (rx >> Talea.dimensionPo2) + ((ry>> Talea.dimensionPo2) << yshift) + ((rz>> Talea.dimensionPo2) << zshift)
			var talea = coreArray(taleaIndex)
			if ( talea == null ) {
				talea = new ByteTalea(VoxelCoord((Vec3i(x,y,z) >> Talea.dimensionPo2) << Talea.dimensionPo2),defaultValue)
				coreArray(taleaIndex) = talea
			}
			talea( x - talea.x , y - talea.y , z - talea.z ) = t
		} else {
			fallbackGrid(x,y,z) = t
		}
	}

	def writeExternal(p1: ObjectOutput) {
	}
	def readExternal(p1: ObjectInput) {
	}
}