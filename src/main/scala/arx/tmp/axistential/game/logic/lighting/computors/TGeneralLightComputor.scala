package arx.axistential.game.logic.lighting.computors

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 7/7/12
 * Time: 12:02 PM
 * Created by nonvirtualthunk
 */

import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.TVoxelStore
import arx.tmp.game.logic.datastructures.TVoxelView

trait TGeneralLightComputor {
	def computeLight ( voxels : TVoxelView[Byte], lightSources : List[AbstractLightSource] , lightValues : TVoxelStore[Byte] , bounds : WorldRegion, voxelsPerFalloff : Int )
}

class AbstractLightSource(var position : VoxelCoord,var strength : Byte) {

}

class BasicGeneralLightComputor extends TGeneralLightComputor {
	var TL_FlightNodeQ = new ThreadLocal[FastLightNodeQueue] { override def initialValue:FastLightNodeQueue = { new FastLightNodeQueue() } }
	def computeLight(voxels: TVoxelView[Byte], lightSources: List[AbstractLightSource], lightValues: TVoxelStore[Byte], bounds: WorldRegion, voxelsPerFalloff: Int) {
//		var x = bounds.min.x;while ( x < bounds.max.x ) {
//			var y = bounds.min.y;while ( y < bounds.max.y ) {
//
//			y += 1}
//		x += 1}

		val lightValueShift = (math.log(voxelsPerFalloff) / math.log(2)).toInt

		val lightNodeQ = TL_FlightNodeQ.get
		lightNodeQ.clear()
		for ( ls <- lightSources ) {
			lightNodeQ.enqueue(ls.position.x,ls.position.y,ls.position.z,Center,ls.strength << lightValueShift)
		}

		while ( lightNodeQ.nonEmpty ) {
			lightNodeQ.dequeue()
			val x = lightNodeQ.x
			val y = lightNodeQ.y
			val z = lightNodeQ.z
			if ( x >= bounds.min.x && x <= bounds.max.x && y >= bounds.min.y && y <= bounds.max.y && z >= bounds.min.z && z <= bounds.max.z ) {
				if ( voxels(x,y,z) == 0.toByte ) {
					val lv = lightNodeQ.lightValue
					val cameFrom = lightNodeQ.cameFrom

					if ( (lv >> lightValueShift) > lightValues(x,y,z) ) {
						lightValues(x,y,z) = (lv >> lightValueShift).toByte

						var q = 0;while ( q < 6 ) {
							if ( q != oppositeDirection(cameFrom) ) {
								val nx = x + cardinalsX(q)
								val ny = y + cardinalsY(q)
								val nz = z + cardinalsZ(q)
								val nlv = lv - 1

								lightNodeQ.enqueue(nx,ny,nz,q,nlv)
							}
						q += 1}
					}
				}
			}
		}
	}
}