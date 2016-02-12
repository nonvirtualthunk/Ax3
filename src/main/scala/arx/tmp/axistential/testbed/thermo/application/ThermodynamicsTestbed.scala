package arx.axistential.testbed.thermo.application

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/19/13
 * Time: 2:41 PM
 */

import arx.Prelude._
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.Vec2f
import arx.core.vec.Vec2i
import arx.core.vec.Vec4f
import arx.core.FibonacciHeap
import arx.core.THasSortKey
import com.carrotsearch.hppc.LongOpenHashSet

import scala.collection.mutable.ArrayBuffer
import scalaxy.loops._

class ThermodynamicsTestbed extends DeadSimpleConflux {
	import arx.axistential.testbed.thermo.application.ThermodynamicsTestbed._
	var dims = Vec2i(100,100)
	var thermoGrid = Array.ofDim[Int](100,100)
	val insulationGrid = Array.ofDim[Float](100,100)
//	for ( dy <- -10 to 10 ) { insulationGrid(60)(50 + dy) = 20.0f }
//	for ( dx <- -10 to 10 ) { insulationGrid(50 + dx)(60) = 20.0f }
//	for ( dx <- -10 to 10 ) { insulationGrid(50 + dx)(40) = 20.0f }

	val xyCardinals = Cardinals.expandedCardinals.filter(_.z == 0)
	val xyScales = xyCardinals.map( v => 1.0f / v.length )
//	val xyLengths = xyCardinals.map( v => powf(v.length,1.3f) )
	val xyLengths = xyCardinals.map( v => powf(v.length,1.0f) )

	var elapsed = zeroSeconds

	def runningSums ( arr : Array[Int] ) = {
		var runningSum = 0
		arr.reverseMap( v => { runningSum += v; runningSum } ).reverse
	}

	val baseBucketSizes = Array(1,8,16,20,24,32,36,40,48,56,60,64,72,76,80,88,96,100,104,112,116,120,128,132,136,144,152,156,160,168,172,176,184,192,196,200,208,212,216,224)
	val baseBucketSums = runningSums(baseBucketSizes)

	println( baseBucketSums.toList )

	def initTherm () {
		val newGrid = Array.ofDim[Int](100,100)

		val maxDist = 40

		val source = Vec2i(50,50)
		val buckets = fillArray(maxDist)(i => new ArrayBuffer[ThermNode])

		val Q = new FibonacciHeap[ThermNode]
		Q.enqueue( ThermNode(source.x,source.y,0) )
		val closed = new LongOpenHashSet(1000)

		while ( Q.nonEmpty && Q.peek.g < maxDist) {
			val node = Q.dequeue()
			val hash = VoxelCoord.hashL(node.x,node.y,0)

//			if ( ! closed.contains(hash) ) {
//				closed.add(hash)
				buckets( node.g.toInt ).append( node )

				for ( q <- 0 until xyCardinals.length optimized ) {
					val ax = node.x + xyCardinals(q).x
					val ay = node.y + xyCardinals(q).y
					val ag = node.g + xyLengths(q) * (insulationGrid(ax)(ay) + 1.0f)

					val adjHash = VoxelCoord.hashL(ax,ay,0)
					if ( ! closed.contains(adjHash) ) {
						closed.add(adjHash)

						Q.enqueue( ThermNode(ax,ay,ag) )
					}
				}
//			}
		}

//		for ( i <- 0 until buckets.length ) {
//			val expected = if(i == 0) { 1 } else { (math.Pi * (i+0.5)*(i+0.5)) - (math.Pi * (i-0.5)*(i-0.5)) }
//			println(s"Bucket $i: ${buckets(i).length} - ${expected.toInt}")
//		}
//		println()
//		buckets.foreach( i => print(s"${i.length},") )
//		println()
		val runSums = runningSums(buckets.map(_.length))
//		runSums.foreach( s => print(s"$s,") )
//		println()
		val ratios = baseBucketSums.zip(runSums).map( tup => tup._1 / tup._2.toFloat )
//		ratios.foreach( ratio=> print(s"$ratio".take(4) + ",") )
//		println()

		println( ratios.head )

		var limit = 1000000
		for ( i <- 0 until buckets.length optimized ) {
			for ( n <- buckets(i) ) {
				val v = ((maxDist - n.g * 1.5) * ratios(i)).toInt.clamp(0,limit)
				limit = math.min(limit,v)
				newGrid(n.x)(n.y) = v
			}
		}
		thermoGrid = newGrid
	}
	initTherm()

	val scale = 0.15f

	def tick(timeElapsed: UnitOfTime): Unit = {
		elapsed += timeElapsed
		if ( elapsed > 0.25.second ) {
			elapsed = zeroSeconds

			timeAndPrint("Thermocompute"){ initTherm() }
		}

		val (start,end) = mainWidget.pixelToWorldSpaceLine(windowingSystem.toPixelCoordinates(windowingSystem.lastWidgetMousePosition),graphicsEngine.pov.viewMatrix,graphicsEngine.pov.projectionMatrix)
		val delta = end - start
		val t = (0.0f - start.z) / (delta.z)
		val p = start + delta * t
		if ( Mouse.isButtonDown(0) ) {
			val x = ((p.x / scale + 0.5f + dims.x*0.5f) ).toInt.clamp(0,dims.x-1)
			val y = ((p.y / scale + 0.5f + dims.y*0.5f) ).toInt.clamp(0,dims.y-1)
			insulationGrid(x)(y) = 10.0f
		}
	}

	def draw(graphics: GraphicsHelper): Unit = {

		for ( x <- 0 until dims.x optimized ; y <- 0 until dims.y optimized ) {
			val gx = (x - dims.x*0.5f) * scale
			val gy = (y - dims.y*0.5f) * scale
			val gv = thermoGrid(x)(y)
			val pcnt = clamp(gv / 145.0f,0.0f,1.0f)
			val color = if ( insulationGrid(x)(y) > 0 ) { Vec4f(0,0,0,1) } else if ( gv >= 0 ) { Vec4f(pcnt,pcnt,pcnt,1.0f) } else { Vec4f(1,0,0,1) }
			graphics.drawQuad(Vec2f(gx,gy),Vec2f(scale,scale),"default/blank.png",color)
		}
	}

	def onKeyPress(kpe: KeyPressEvent): Unit = {}
	def onKeyRelease(kpe: KeyReleaseEvent): Unit = {}
	def onMousePress(mpe: MousePressEvent): Unit = {


	}
	def onMouseRelease(mre: MouseReleaseEvent): Unit = {}
	def onMouseMove(mme: MouseMoveEvent): Unit = {}
	def onMouseDrag(mde: MouseDragEvent): Unit = {}
}

object ThermodynamicsTestbed {
	case class ThermNode ( x : Int , y : Int , g : Float ) extends THasSortKey {
		def sortKey: Float = g
	}
}
	
	
	
	
	/*
	val adjV = Array.ofDim[Int](xyCardinals.length)
			val xDir = rand(0,2)
			val startX = xDir match { case 0 => 0 ; case 1 => dims.x - 1 }
			val endX = xDir match { case 0 => dims.x - 1 ; case 1 => 0 }
			val byX = xDir match { case 0 => 1 ; case 1 => -1 }
			val yDir = rand(0,2)
			val startY = yDir match { case 0 => 0 ; case 1 => dims.y - 1 }
			val endY = yDir match { case 0 => dims.y - 1 ; case 1 => 0 }
			val byY = yDir match { case 0 => 1 ; case 1 => -1 }

			for ( x <- startX to endX by byX; y <- startY to endY by byY ) {
				var current = thermoGrid(x)(y)
				if ( current != 100 ) {

					var pushSum = 0.0f
					var pullSum = 0.0f
					for ( q <- 0 until 8 optimized ) {
						val ax = x + xyCardinals(q).x
						val ay = y + xyCardinals(q).y
						if ( ax >= 0 && ay >= 0 && ax < dims.x && ay < dims.y ) {
							adjV(q) = thermoGrid(ax)(ay)
						} else { adjV(q) = -1 }
					}

//					val avg = (adjV.sum + current) / 5.0f
					val pulls = adjV.filter( _ > current )
					val avg = (pulls.sum + current) / (pulls.size + 1.0f)
					val absDelta = math.abs(avg - current)

					for ( q <- 0 until 8 ) {
						if ( adjV(q) != -1 ) {
							if ( adjV(q) > avg ) { pullSum += (adjV(q) - avg) * xyScales(q) }
							else { pushSum += avg - adjV(q) }
						}
					}

					if ( absDelta > 0 ) {
//						thermoGrid(x)(y) = avg.toInt
						for ( q <- 0 until 8 optimized ) {
							val ax = x + xyCardinals(q).x
							val ay = y + xyCardinals(q).y
							if ( ax >= 0 && ay >= 0 && ax < dims.x && ay < dims.y ) {
								if ( adjV(q) > avg ) {
//									if ( adjV(q) != 100 ) {
										var pcnt = (adjV(q) - avg) / pullSum
//										if ( pushSum != 0.0f ) { pcnt *= 2.0f }

										val delta = absDelta * pcnt * xyScales(q)
									if ( adjV(q) != 100 ) {
										thermoGrid(ax)(ay) = thermoGrid(ax)(ay) - delta.toInt
									}
										thermoGrid(x)(y) += delta.toInt
//									}
//								} else if ( adjV(q) < avg ) {
//									var pcnt = (avg - adjV(q)) / pushSum
//	//								if ( pushSum != 0.0f ) { pcnt *= 2.0f }
//
//									val delta = absDelta * pcnt
//									thermoGrid(ax)(ay) += delta.toInt
								}
							}
						}
					}
				}
			}
	 */
