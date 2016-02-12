package arx.tmp.game.logic.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 4/22/12
 * Time: 5:19 PM
 * Created by nonvirtualthunk
 */

import arx.core.vec.functions._
import arx.core.vec.Vec3f
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.MutableObjectCoord
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord

object TaleaGridUtil {
	def intersect (blocks : TVoxelView[Byte] , centerf : Vec3f, start_oc : ObjectCoord,baseEnd_oc : ObjectCoord,acceptFunction : (WorldIntersectionResult) => Boolean, auxGrid:TVoxelView[Byte] = DummyInfiniteByteVoxelStore ) : Option[WorldIntersectionResult] = {
		val start = start_oc + centerf
		val baseEnd = baseEnd_oc + centerf

		val end = MutableObjectCoord(start + normalize(baseEnd - start) * 128.0f)
//		Noto.info("Start : " + start + " end : " + end)
		val totalDelta = end - start

//		val starti = Vec3i(start)
//		val endi = Vec3i(end)
//
//		var result : Option[WorldIntersectionResult] = None
//
//		var lastVoxel : Byte = 0.toByte
//		var lastPoint = Vec3i(0,0,0)
//		def processPoint ( x : Int , y : Int , z : Int ) : Boolean = {
//			Noto.info("Processing : "+ x + "," + y + "," + z)
//			val curVoxel = blocks(x,y,z)
//			if ( curVoxel > 0.toByte && lastVoxel == 0.toByte ) {
//				var face = if ( lastPoint.x > x ) { Cardinals.Right }
//				else if ( lastPoint.x < x ) { Cardinals.Left }
//				else if ( lastPoint.y > y ) { Cardinals.Front }
//				else if ( lastPoint.y < y ) { Cardinals.Back }
//				else if ( lastPoint.z > z ) { Cardinals.Top }
//				else { Cardinals.Bottom }
////				var face = 0
////				var bestFace = -1
////				var bestU = 10000.0f
////				while ( face < 6 ) {
////					val u = (face match {
////						case Cardinals.Left => 	((x - starti.x).toFloat-0.5f) / (totalDelta.x)
////						case Cardinals.Right => 	((x - starti.x).toFloat+0.5f) / (totalDelta.x)
////						case Cardinals.Back => 	((y - starti.y).toFloat-0.5f) / (totalDelta.y)
////						case Cardinals.Front => 	((y - starti.y).toFloat+0.5f) / (totalDelta.y)
////						case Cardinals.Bottom => 	((z - starti.z).toFloat-0.5f) / (totalDelta.z)
////						case Cardinals.Top => 		((z - starti.z).toFloat+0.5f) / (totalDelta.z)
////						case _ => 100000.0f
////					})
////					if ( u > 0.0f && u < bestU ) {
////						bestFace = face
////						bestU = u
////					}
////				face += 1}
//
////				result = Some(WorldIntersectionResult(VoxelCoord(x,y,z),ObjectCoord(start_oc + totalDelta * bestU),bestFace))
//				result = Some(WorldIntersectionResult(VoxelCoord(x,y,z),ObjectCoord(start_oc),face))
//				false
//			} else {
//				lastPoint = Vec3i(x,y,z)
//				lastVoxel = curVoxel
//				true
//			}
//		}
//
//		bresenhams(starti,endi,processPoint)
//
//		result





		for ( i <- 0 until 3 ) { if ( end(i) == start(i) ) { end(i) += 0.000001f } } //prevent NaN's down the line
		val l = length(end - start)
		val delta = normalize(end - start) * 0.5f
		val pos = Vec3f(start)

		var f = 0.0f;
		while ( f < 156.0f ) {
			var best = (10000000000.0f,Vec3i(0,0,0),-1) //depth,pos,face
			var tx = -1;while ( tx <= 1 ) {
				var ty = -1;while ( ty <= 1 ) {
					var tz = -1;while ( tz <= 1 ) {
						val px = pos.x.toInt + tx; val py = pos.y.toInt + ty; val pz = pos.z.toInt + tz
						val vox = blocks(px,py,pz)
						if ( vox > 0 || auxGrid(px,py,pz) > 0 ) {
							var face = 0
							while ( face < 6 ) {
								val u = (face match {
									case Cardinals.Left => 	(scala.math.floor	(pos.x + tx.toFloat).toFloat - start.x) / (totalDelta.x)
									case Cardinals.Right =>	(scala.math.ceil	(pos.x + tx.toFloat).toFloat - start.x) / (totalDelta.x)
									case Cardinals.Back => 	(scala.math.floor	(pos.y + ty.toFloat).toFloat - start.y) / (totalDelta.y)
									case Cardinals.Front => 	(scala.math.ceil	(pos.y + ty.toFloat).toFloat - start.y) / (totalDelta.y)
									case Cardinals.Bottom => 	(scala.math.floor	(pos.z + tz.toFloat).toFloat - start.z) / (totalDelta.z)
									case Cardinals.Top => 		(scala.math.ceil	(pos.z + tz.toFloat).toFloat - start.z) / (totalDelta.z)
									case _ => 100000.0f
								})
								if ( u > 0.0f && u < best._1 ) {
									val theoreticalPos = start + totalDelta * u - (Cardinals.cubeFaceNormals(face) * 0.01f)
									//We can't have negatives, it doesn't make sense given that a talea grid must be origin'd at (0,0,0)
									if ( theoreticalPos.x >= 0.0f && theoreticalPos.y >= 0.0f && theoreticalPos.z >= 0.0f ) {
										val eqx = theoreticalPos.x.toInt == pos.x.toInt + tx
										val eqy = theoreticalPos.y.toInt == pos.y.toInt + ty
										val eqz = theoreticalPos.z.toInt == pos.z.toInt + tz

										if ( eqx && eqy && eqz &&
											(blocks(theoreticalPos.x.toInt,theoreticalPos.y.toInt,theoreticalPos.z.toInt) > 0 || auxGrid(theoreticalPos.x.toInt,theoreticalPos.y.toInt,theoreticalPos.z.toInt) > 0) &&
											(blocks(theoreticalPos.x.toInt + cardinalsX(face),theoreticalPos.y.toInt + cardinalsY(face),theoreticalPos.z.toInt + cardinalsZ(face)) == 0 &&
												auxGrid(theoreticalPos.x.toInt + cardinalsX(face),theoreticalPos.y.toInt + cardinalsY(face),theoreticalPos.z.toInt + cardinalsZ(face)) == 0) )
										{
											if ( acceptFunction == null || acceptFunction(WorldIntersectionResult(VoxelCoord(Vec3i(theoreticalPos)),ObjectCoord((start + totalDelta *u) - centerf),face).withVoxelValue(vox)) ) {
												best = (u,Vec3i(theoreticalPos),face)
											}
										}
									}
								}
								face += 1
							}
						}
					tz += 1}
				ty += 1}
			tx += 1}

			if ( best._3 != -1 ) {
//				Noto.info("Hit " + best._2 + " at face " + best._3 + " at point " + ((start + totalDelta * best._1)))
				val adj = best._2 + Cardinals.cardinals(best._3)
//				Noto.info("   adj block : " + blocks(adj.x,adj.y,adj.z))
				return Some(WorldIntersectionResult(VoxelCoord(best._2),ObjectCoord((start + totalDelta * best._1) - centerf),best._3).withVoxelValue(blocks(best._2.x,best._2.y,best._2.z)).withAuxGridValue(auxGrid(best._2.x,best._2.y,best._2.z)))
			}

			pos.x += delta.x
			pos.y += delta.y
			pos.z += delta.z
			f += 0.5f
		}
		None
	}

	def signOf ( a : Int ) = if ( a > 0 ) { 1 } else if ( a < 0 ) { -1 } else { 0 }

	def bresenhams ( start : Vec3i , end : Vec3i , callback : (Int,Int,Int) => Boolean ) {
		val dx = end.x - start.x
		val dy = end.y - start.y
		val dz = end.z - start.z

		val ax = math.abs(dx) << 1
		val ay = math.abs(dy) << 1
		val az = math.abs(dz) << 1

		val sx = signOf(dx)
		val sy = signOf(dy)
		val sz = signOf(dz)

		var x = start.x
		var y = start.y
		var z = start.z

		if ( ax >= math.max(ay,az) ) {
			var yd = ay - (ax >> 1)
			var zd = az - (ax >> 1)
			while (true) {
				if ( ! callback(x,y,z) ) { return }
				if ( x == end.x ) { return }
				if ( yd >= 0 ) {
					y += sy
					yd -= ax
				}
				if ( zd >= 0 ) {
					z += sz
					zd -= ax
				}

				x += sx
				yd += ay
				zd += az
			}
		} else if ( ay >= math.max(ax,az) ) {
			var xd = ax - (ay >> 1)
			var zd = az - (ay >> 1)
			while ( true ) {
				if ( ! callback(x,y,z) ) { return }
				if ( y == end.y ) { return }
				if ( xd >= 0 ) {
					x += sx
					xd -= ay
				}
				if ( zd >= 0 ) {
					z += sz
					zd -= ay
				}

				y += sy
				xd += ax
				zd += az
			}
		} else if ( az >= math.max(ax,ay) ) {
			var xd = ax - (az >> 1)
			var yd = ay - (az >> 1)
			while ( true ) {
				if ( ! callback(x,y,z) ) { return }
				if ( z == end.z ) { return }
				if ( xd >= 0 ) {
					x += sx
					xd -= az
				}
				if ( yd >= 0 ) {
					y += sy
					yd -= az
				}

				z += sz
				xd += ax
				yd += ay
			}
		}
	}
}

