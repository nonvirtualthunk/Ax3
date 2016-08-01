package arx.eldr.game.logic.light

import arx.core.vec.Cardinals
import arx.core.vec.Vec3i
import arx.eldr.game.world.data.Light
import arx.eldr.game.world.data.Terrain
import arx.engine.world.World

import scala.collection.mutable

/**
 *
 */

abstract class TLightComputor(world : World) {
	val terrainData = world[Terrain]
	val lightData = world[Light]
	
	case class LightNode (x : Int, y : Int, z : Int, cameFrom : Int, lightValue : Int) {
		def pos (axis : Int) = axis match {
			case 0 => x
			case 1 => y
			case 2 => z
		}
	}
	val zeroByte = 0.toByte
	val oppositeSideIndices = Array(Cardinals.Right,Cardinals.Front,Cardinals.Top,Cardinals.Left,Cardinals.Back,Cardinals.Bottom,Cardinals.Center)
	var TLlightNodeQ = new ThreadLocal[mutable.Queue[LightNode]] { override def initialValue:mutable.Queue[LightNode] = { new mutable.Queue[LightNode]() } }
	var TLrelightQ = new ThreadLocal[mutable.Queue[Vec3i]] { override def initialValue:mutable.Queue[Vec3i] = { new mutable.Queue[Vec3i]() } }
	var TLfullLightQ = new ThreadLocal[mutable.Queue[LightNode]] { override def initialValue:mutable.Queue[LightNode] = { new mutable.Queue[LightNode]() } }
	val TLreplacementLightQ = new ThreadLocal[ReplacementLightNodeQueue] { override def initialValue:ReplacementLightNodeQueue = { new ReplacementLightNodeQueue() } }
//	def transparency(b : Byte) = if ( b == 0.toByte ) { 0 } else if ( b < 0.toByte ) { -8 } else { 1 }// == 0.toByte
	val cardinals = Cardinals.cardinals.clone()
	val Bottom = Cardinals.Bottom

	def highestAdjacentLightValue ( x : Int ,y : Int, z : Int, gli : Int, fullLight : Byte ) : Byte

	class ReplacementLightNodeQueue {
		val x = Array.ofDim[Int](5000)
		val y = Array.ofDim[Int](5000)
		val z = Array.ofDim[Int](5000)
		val expectedLV = Array.ofDim[Int](5000)
		val replacementLV = Array.ofDim[Int](5000)
		val cameFrom = Array.ofDim[Int](5000)
		var idx = -1

		def enqueue ( ax : Int , ay : Int, az : Int , e : Int , r : Int , q : Int ) {
			idx += 1
			x(idx) = ax
			y(idx) = ay
			z(idx) = az
			expectedLV(idx) = e
			replacementLV(idx) = r
			cameFrom(idx) = q
		}
		def dequeue () {
			idx -= 1
		}
		def nonEmpty : Boolean = { idx >= 0 }
	}
}