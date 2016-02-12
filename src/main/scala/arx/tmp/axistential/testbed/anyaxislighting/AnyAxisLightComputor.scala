package arx.axistential.testbed.anyaxislighting

import arx.Prelude._
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.logic.lighting.computors.FastLightNodeQueue
import arx.axistential.game.logic.lighting.computors.TGlobalLightComputor
import arx.core.datastructures.primitive.ShortArray2D
import arx.core.vec.coordinates.MutableVoxelCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.ReadVec2i
import arx.core.vec.ReadVec3i
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.ITalea
import arx.tmp.game.logic.datastructures.TTaleaGrid
import arx.tmp.game.logic.datastructures.Talea
import arx.tmp.game.logic.world.data.LightData
import arx.tmp.game.logic.world.data.LightData.LightTaleaType

import scalaxy.loops._

class AnyAxisLightComputor extends TGlobalLightComputor {
	var TL_LightNodeQueue = new ThreadLocal[FastLightNodeQueue] { override def initialValue:FastLightNodeQueue = { new FastLightNodeQueue() } }

	case class Context (world : World,primaryQ : Int,gli : Int, LD : LightData, TD : TerrainData,orthoAxis : Int,upAxis : Int,mainAxis : Int) {
		val negativeQ = primaryQ / 3 == 0
		val qSign = if (negativeQ) { -1 } else { 1 }
		val mDelta = qSign
		val mStart = if(negativeQ) {Talea.dimension - 1} else {0}
		val mEnd = if(negativeQ) {-1} else {Talea.dimension}
	}
	val uninterruptedGrid = new ShortArray2D(Talea.dimension,Talea.dimension)

	override def preInitializeLighting(env: World, primaryQ: Int, gli: Int): Unit = {
		Timed {
			val TD = env.aux[TerrainData]
			val LD = env.aux[LightData]

			val orthoAxis = (primaryQ + 1) % 3
			val upAxis = (primaryQ + 2) % 3
			val mainAxis = primaryQ % 3

			implicit val ctxt = Context(env,primaryQ,gli,LD,TD,orthoAxis,upAxis,mainAxis)

			val negativeQ = primaryQ / 3 == 0
			val reg = env.worldRegionAsSpatialRegion

			var originTaleae = List[VoxelCoord]()

			// o = ortho, u = up, m = main
			for (	o <- reg.lowerCorner(orthoAxis) to reg.upperCorner(orthoAxis) by Talea.dimension;
					u <- reg.lowerCorner(upAxis) to reg.upperCorner(upAxis) by Talea.dimension)
			{
				// if the primary direction is negative, we start at the top, otherwise we start at the bottom
				val m = if (negativeQ) { reg.upperCorner(mainAxis) } else { reg.lowerCorner(mainAxis) }
				val v = VoxelCoord(orthoAxis -> o, upAxis -> u, mainAxis -> m)
				originTaleae ::= v
			}

			val interruptionGrids = originTaleae.map(preInitOriginTalea)
			originTaleae.zip(interruptionGrids).par.foreach {
				case (pos,interGrid) => initOriginTalea(pos,interGrid)
			}
		}.println("pre-init : ","s")
	}

	def preInitOriginTalea (startPos : VoxelCoord)(implicit ctxt : Context) : ShortArray2D = {
		import ctxt._

		val minMain = world.worldRegionAsSpatialRegion.lowerCorner(mainAxis)
		val maxMain = world.worldRegionAsSpatialRegion.upperCorner(mainAxis)

		var pos = startPos
		val maxUninterruptedColumns = Talea.dimension * Talea.dimension
		var uninterruptedColumns = maxUninterruptedColumns
		val interruptionGrid = new ShortArray2D(Talea.dimension,Talea.dimension)

		while (pos(mainAxis) >= minMain && pos(mainAxis) <= maxMain) {
			val terrain = TD.materialGrid.taleaFor(pos.x,pos.y,pos.z)
			val light = LD.globalLighting(gli).taleaFor(pos.x,pos.y,pos.z)
			val fullLight = LD.FullLight
			if (uninterruptedColumns == maxUninterruptedColumns && terrain.areAll(0)) {
				// then we can pass straight through, they are guaranteed to all be fully lit
				light.defaultValue = fullLight
				light.nonDefaultCount = 0
			} else {
				light.defaultValue = 0
				// if there aren't any uninterrupted columns, we don't have to check them individually
				if (uninterruptedColumns > 0) {
					val v = MutableVoxelCoord(0,0,0)
					for (o <- 0 until Talea.dimension optimized ) {
						v(orthoAxis) = o
						for (u <- 0 until Talea.dimension optimized) {
							v(upAxis) = u
							// if this column has not yet been interrupted
							if (interruptionGrid(o,u) == 0) {
								// move down the column until we hit something non-transparent
								var m = mStart; while (m != mEnd) {
									v(mainAxis) = m
									if (transparency(terrain(v.x,v.y,v.z)) == 0) {
										light(v.x,v.y,v.z) = fullLight
										m = m + mDelta
									} else {
										light(v.x,v.y,v.z) = -1.toByte
										uninterruptedColumns -= 1
										interruptionGrid(o,u) = (pos(mainAxis) + m).toShort
										m = mEnd
									}
								}
							}
						}
					}
				}
			}

			// move to the next talea in our column
			pos = pos.plusAxis(mainAxis,mDelta * Talea.dimension)
		}

		// if there were no interruptions at all, we re-use the sentinel grid
		if (uninterruptedColumns == maxUninterruptedColumns) {
			uninterruptedGrid
		// otherwise return the grid indicating at what point in each column an interruption was found
		} else {
			interruptionGrid
		}
	}

	def initOriginTalea (startPos : VoxelCoord, interruptionGrid : ShortArray2D)(implicit ctxt : Context): Unit = {
		import ctxt._

		val minMain = world.worldRegionAsSpatialRegion.lowerCorner(mainAxis)
		val maxMain = world.worldRegionAsSpatialRegion.upperCorner(mainAxis)

		var pos = startPos

		val adjDeltas = Array(ReadVec2i(-1,0),ReadVec2i(0,-1),ReadVec2i(1,0),ReadVec2i(0,1))

		val Q = TL_LightNodeQueue.get()
		while (pos(mainAxis) >= minMain && pos(mainAxis) <= maxMain) {
			val terrain = TD.materialGrid.windowCenteredOnTaleaAt(pos,readOnly = true)
			val light = LD.globalLighting(gli).windowCenteredOnTaleaAt(pos,readOnly = false)
			val cPos = terrain.centerTalea.position
			val fullLight = LD.FullLight
//			if (! light.centerTalea.areAll(fullLight)) {
				val v = MutableVoxelCoord(0,0,0)
				val absolute_mStart = mStart + cPos (mainAxis)
				for (o <- 0 until Talea.dimension optimized ) {
					v(orthoAxis) = o
					for (u <- 0 until Talea.dimension optimized) {
						v(upAxis) = u
						val edge = o == 0 || o == Talea.dimension - 1 || u == 0 || u == Talea.dimension - 1

						val curColInterruptedAt = interruptionGrid(o,u) match {
							case 0 => if (negativeQ) {minMain} else {maxMain}
							case other => other
						}
						val localCurCol = curColInterruptedAt - cPos(mainAxis)

						// if this column has not already been interrupted prior to the start of this talea
						if ((negativeQ && curColInterruptedAt < absolute_mStart) || (!negativeQ && curColInterruptedAt > absolute_mStart) ) {
							for (adji <- 0 until 4 optimized) {
								val adjDelta = adjDeltas(adji)
								val av = MutableVoxelCoord(v)
								av(orthoAxis) += adjDelta.x
								av(upAxis) += adjDelta.y
								if (!edge) {
									val adjInterruption = interruptionGrid(o + adjDelta.x,u + adjDelta.y)
									// we start one back from the interruption point
									val fromM = clamp(localCurCol - mDelta,0,Talea.dimension-1)
									// and we go until we hit our adjacent column's interruption point
									val localAdjInterrupt = adjInterruption - cPos(mainAxis)
									val toM = clamp(localAdjInterrupt,0,Talea.dimension)
									
									// if the to is in the correct direction
									if (sign(fromM - toM) == qSign) {
										var m = fromM
										// TODO: toM inclusive?
										while (m != toM) {
											av(mainAxis) = m

											if (transparency(terrain.centerTalea(av.x,av.y,av.z)) == 0) {
												light(av.x,av.y,av.z) = (fullLight - 1).toByte
												Q.enqueue(av.x,av.y,av.z,Center,fullLight - 2)
											}

											m -= mDelta
										}
									}
								} else {
									val interruptM = clamp(localCurCol,0,Talea.dimension)
									var m = mStart; while (m != interruptM + mDelta) {
										av(mainAxis) = m

										if (transparency(terrain(av.x,av.y,av.z)) == 0 && light(av.x,av.y,av.z) < fullLight) {
											light(av.x,av.y,av.z) = (fullLight - 1).toByte
											Q.enqueue(av.x,av.y,av.z,Center,fullLight - 2)
										}

										m += mDelta
									}
								}
							}
						}
					}
				}
//			}

			while (Q.nonEmpty) {
				Q.dequeue()

				light(Q.x,Q.y,Q.z) = Q.lightValue.toByte
				for (q <- 0 until 6 optimized) {
					if (q != oppositeDirection(Q.cameFrom)) {
						val ax = Q.x + cardinalsX(q)
						val ay = Q.y + cardinalsY(q)
						val az = Q.z + cardinalsZ(q)

						val transp = transparency(terrain(ax,ay,az))
						if (transp == 0) {
							val newLV = Q.lightValue - 2
							val oldLV = light.setIfGreater(ax,ay,az,newLV.toByte)
							if (oldLV < newLV) {
								Q.enqueue(ax,ay,az,q,newLV)
							}
						}
					}
				}

			}

			// move to the next talea in our column
			pos = pos.plusAxis(mainAxis,mDelta * Talea.dimension)
		}
	}

	override def preInitializeLightingForTalea(env: World, loc: ReadVec3i, primaryQ: Int, gli: Int): Unit = {

	}

	override def updateLightingForTalea(env: World, lightGrid: TTaleaGrid[Byte, LightTaleaType], lightTalea: LightTaleaType, primaryQ: Int, gli: Int, localLighting: Boolean): Set[ITalea[_]] = {
		Set()
	}

	override def initializeLightingForTalea(env: World, loc: ReadVec3i, primaryQ: Int, gli: Int): Unit = {

	}
}
