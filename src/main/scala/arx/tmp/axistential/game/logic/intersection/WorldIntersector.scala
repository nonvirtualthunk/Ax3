package arx.axistential.game.logic.intersection

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/9/13
 * Time: 10:58 AM
 * To change this template use File | Settings | File Templates.
 */

import arx.axistential.game.data.world.TerrainData
import arx.core.vec.Cardinals
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.Vec3f
import arx.core.vec.Vec3i
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.TInfiniteVoxelView

object WorldIntersector {
	def intersect ( world : World, start_oc : ObjectCoord, baseEnd_oc : ObjectCoord , auxGrid : TInfiniteVoxelView[Byte], filterFunction : (WorldIntersectionResult) => Boolean ) : Option[WorldIntersectionResult] = {
		val centerf = world.centerf
		val start = start_oc + centerf
		val baseEnd = baseEnd_oc + centerf
		val terrain = world.aux[TerrainData]

		val end = Vec3f(start + (baseEnd - start).normalizeSafe * 156.0f)
		val totalDelta = end - start
		for ( i <- 0 until 3 ) { if ( end(i) == start(i) ) { end(i) += 0.000001f } } //prevent NaN's down the line
		val l = (end - start).lengthSafe
		val delta = (end - start).normalizeSafe * 0.5f
		val pos = Vec3f(start)

		var f = 0.0f
		while ( f < 156.0f ) {
			var best = (10000000000.0f,Vec3i(0,0,0),-1) //depth,pos,face
			var tx = -1;while ( tx <= 1 ) {
				var ty = -1;while ( ty <= 1 ) {
					var tz = -1;while ( tz <= 1 ) {
						val px = pos.x.toInt + tx; val py = pos.y.toInt + ty; val pz = pos.z.toInt + tz
						val vox = terrain.materialByteAt(px,py,pz)
						val aux = auxGrid(px,py,pz)
						if ( vox != 0 || aux != 0 ) {
							var face = 0
							while ( face < 6 ) {
								val u = (face match {
									case Cardinals.Left => (scala.math.floor(pos.x + tx.toFloat).toFloat - start.x) / (totalDelta.x)
									case Cardinals.Right => (scala.math.ceil(pos.x + tx.toFloat).toFloat - start.x) / (totalDelta.x)
									case Cardinals.Back => (scala.math.floor(pos.y + ty.toFloat).toFloat - start.y) / (totalDelta.y)
									case Cardinals.Front => (scala.math.ceil(pos.y + ty.toFloat).toFloat - start.y) / (totalDelta.y)
									case Cardinals.Bottom => (scala.math.floor(pos.z + tz.toFloat).toFloat - start.z) / (totalDelta.z)
									case Cardinals.Top => (scala.math.ceil(pos.z + tz.toFloat).toFloat - start.z) / (totalDelta.z)
									case _ => 100000.0f
								})
								if ( u < best._1 ) {
									val theoreticalPos = start + totalDelta * u - (Cardinals.cubeFaceNormals(face) * 0.01f)
									val eqx = theoreticalPos.x.toInt == pos.x.toInt + tx
									val eqy = theoreticalPos.y.toInt == pos.y.toInt + ty
									val eqz = theoreticalPos.z.toInt == pos.z.toInt + tz

									val vtp = Vec3i(theoreticalPos)
									if ( eqx && eqy && eqz &&
											(terrain.materialByteAt(vtp.x,vtp.y,vtp.z) != 0 || auxGrid(vtp.x,vtp.y,vtp.z) != 0) &&
											( terrain.materialByteAt(vtp.x + cardinalsX(face),vtp.y + cardinalsY(face),vtp.z + cardinalsZ(face)) == 0 &&
												auxGrid(vtp.x + cardinalsX(face),vtp.y + cardinalsY(face),vtp.z + cardinalsZ(face)) == 0) )
									{
										if ( filterFunction(WorldIntersectionResult(VoxelCoord(Vec3i(theoreticalPos)),ObjectCoord((start + totalDelta *u) - centerf),face).withVoxelValue(vox).withAuxGridValue(aux)) ) {
											best = (u,Vec3i(theoreticalPos),face)
										}
									}
								}
								face += 1}
						}
						tz += 1}
					ty += 1}
				tx += 1}

			if ( best._3 != -1 ) {
				return Some(WorldIntersectionResult(VoxelCoord(best._2),ObjectCoord((start + totalDelta * best._1) - centerf),best._3).withVoxelValue(terrain.materialByteAt(best._2.x,best._2.y,best._2.z)).withAuxGridValue(auxGrid(best._2.x,best._2.y,best._2.z)))
			}

			pos.x += delta.x
			pos.y += delta.y
			pos.z += delta.z
			f += 0.5f}

		None
	}
}
