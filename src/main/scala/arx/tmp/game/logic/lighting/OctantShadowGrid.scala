package arx.tmp.game.logic.lighting

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/21/13
 * Time: 8:33 AM
 * Created by nonvirtualthunk
 */

import java.util

import arx.core.vec.coordinates.VoxelCoord

class OctantShadowGrid extends TShadowGrid {
//	val leftBackBottom = new GenericTalea[Short](Vec3i(0,0,0),0.toShort)
	val allShadows = Array.ofDim[Byte](64*64*64)

	@inline
	def apply ( x : Int, y : Int, z : Int ) : Float = {
		allShadows( ((x+32) << 12) + ((y+32) << 6) + (z+32) ).toFloat / 100.0f
	}
	@inline
	def update ( x : Int, y : Int, z : Int , f : Float ) {
		allShadows( ((x+32) << 12) + ((y+32) << 6) + (z+32) ) = (f * 100).toByte
	}

	@inline
	def setIfGreater ( x : Int, y : Int, z : Int , f : Float ) {
		val index = ((x+32) << 12) + ((y+32) << 6) + (z+32)
		allShadows( index ) = math.max( allShadows(index) , f * 100 ).toByte
	}

	@inline
	def raw ( x : Int, y : Int, z : Int ) : Byte = {
		allShadows( ((x+32) << 12) + ((y+32) << 6) + (z+32) )
	}
	@inline
	def setRaw ( x : Int, y : Int, z : Int , b : Byte ) {
		allShadows( ((x+32) << 12) + ((y+32) << 6) + (z+32) ) = b
	}

	def clear () { util.Arrays.fill(allShadows,0.toByte) }
	def clearTo ( b : Byte ) { util.Arrays.fill(allShadows,b) }
}

class InfiniteOctantShadowGrid(shadowGrid : OctantShadowGrid, center : VoxelCoord) extends TInfiniteShadowGrid {
	def apply(x: Int, y: Int, z: Int): Float = {
		val rx = x - center.x
		val ry = y - center.y
		val rz = z - center.z

		if ( rx <= -32 || rx >= 32 || ry <= -32 || ry >= -32 || rz <= -32 || rz >= -32 ) {
			0.0f
		} else {
			shadowGrid(rx,ry,rz)
		}
	}
}