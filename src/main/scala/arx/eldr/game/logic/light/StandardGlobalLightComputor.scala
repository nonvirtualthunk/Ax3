package arx.eldr.game.logic.light

/**
 * TODO: Add javadoc
 */
import arx.Prelude._
import arx.application.Noto
import arx.core.datastructures.voxel.TTalea
import arx.core.datastructures.voxel.Talea
import arx.core.datastructures.voxel.VoxelGrid
import arx.core.datastructures.voxel.VoxelWindow
import arx.core.vec.Cardinals
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.MutableVoxelCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.eldr.game.world.data.Light
import arx.engine.world.World
import scala.collection.mutable
import scalaxy.loops._

class StandardGlobalLightComputor(worldArg : World) extends TGlobalLightComputor(worldArg) {
	var TLFlightNodeQ = new ThreadLocal[FastLightNodeQueue] { override def initialValue:FastLightNodeQueue = { new FastLightNodeQueue() } }
	var TLFfullLightQ = new ThreadLocal[FastLightNodeQueue] { override def initialValue:FastLightNodeQueue = { new FastLightNodeQueue() } }
	var TaleaLengthCardinals = cardinals.map(_ * Talea.dimension).toArray


	def transparency(s : Short) =
		if (terrainData.materialMapping(s).byteTransparency != 0) {
			1
		} else {
			0
		}

	def preInitializeLightingForTalea(lightTaleaPos: VoxelCoord, primaryQ : Int, gli : Int) {
		Noto.debug("Pre Initializing lighting at : " + lightTaleaPos)

		//If the talea above and the talea we are interested in are both empty, the whole thing is guaranteed
		//to be fully lit. We may want this to extend outward a bit farther up, at some point...soon
		var setAllTo : Option[Byte] = None
		val terrain = terrainData.material
		val centerTerrain = terrain.taleaAtRO( lightTaleaPos.x , lightTaleaPos.y ,lightTaleaPos.z )

		if ( centerTerrain.areNone(0.toByte) ) {
			setAllTo = Some(-1.toByte)
		} else {
			if ( centerTerrain.areAll(0.toByte) ) {
				var sourceTaleaLoc = lightTaleaPos
				var iter = 0
				var allClear = true

				// check if all the terrain blocks in the direction of origination are empty, out 4 talea lengths
				while (iter < 8 && allClear) {
					sourceTaleaLoc = sourceTaleaLoc - cardinals(primaryQ) * Talea.dimension
					// the terrain from which unattenuated light will be originating, above in the case of a noon-day sun
					val sourceTerrain = terrain.taleaAtRO(VoxelCoord(sourceTaleaLoc))
					if (! sourceTerrain.areAll(0.toByte)) {
						allClear = false
					}
					iter += 1
				}

				if (allClear) {
					setAllTo =  Some(lightData.globalFullLight(gli))
				}
			}
		}

		val lightTalea = lightData.global(gli).taleaAt(lightTaleaPos.x,lightTaleaPos.y,lightTaleaPos.z)
		setAllTo match {
			case Some(b) =>
				lightTalea.setAll(b)

				val lastUpdatedMark = math.max(0,centerTerrain.modifiedCount)
				lightTalea[LightTaleaData].lastTriggered = lastUpdatedMark
				lightTalea[LightTaleaData].lastUpdated = lastUpdatedMark
			case None =>
				lightTalea.setAll(0)
		}
	}

	def initializeLightingForTalea(lightTaleaPos: VoxelCoord, primaryQ : Int, gli : Int) {
		initializeLightingForTalea(lightTaleaPos,0, primaryQ, gli)
	}
	def initializeLightingForTalea(lightTaleaPos: VoxelCoord,recurseGuard:Int, primaryQ : Int, gli : Int) {
		val lightGrid = lightData.global(gli)
		val lightWindow = VoxelWindow.centeredOnTalea(lightGrid, lightTaleaPos)
		val terrain = terrainData.material

		val lightTalea = lightGrid.taleaAt(VoxelCoord(lightTaleaPos))
		if (lightTalea[LightTaleaData].lastUpdated >= 0) {
			return
		} //already initialized, ignore
		val lightNodeQ: FastLightNodeQueue = TLFlightNodeQ.get()
		val fullLightQ: FastLightNodeQueue = TLFfullLightQ.get()



		if ( recurseGuard > 20 ) {
			Noto.warn (s"hit recurse guard at $lightTaleaPos")
			val mark = terrainData.material.taleaAtRO(lightTalea.position).modifiedCount
			lightTalea[LightTaleaData].lastUpdated = mark
			lightTalea[LightTaleaData].lastTriggered = mark
			lightTalea.setAll(lightData.globalFullLight(gli))
			return
		}

		val oppositePrimaryQ = Cardinals.oppositeDirection(primaryQ)

		// source of unattenuated light, will be opposite the primaryQ direction
		val sourceLightTalea = lightGrid.taleaAtRO(lightTaleaPos + TaleaLengthCardinals(oppositePrimaryQ))
		if (sourceLightTalea[LightTaleaData].lastUpdated < 0) {
			//that is, it's never been updated at all
			/* Recurse into the top talea, this way we can ensure that the one above us has been calculated
when performing the rest of the algorithm, this is necessary because natural light does not attenuate
when moving downard, so effects can propagate more than 1 talea down*/
			Noto.debug("Recursing into top talea : " + sourceLightTalea.position)
			initializeLightingForTalea(sourceLightTalea.position,recurseGuard + 1, primaryQ, gli)
		}

		Noto.debug("Updating lighting at : " + lightTaleaPos)

		val terrainWindow = VoxelWindow.centeredOnTalea(terrain, lightTaleaPos)

		val lastUpdatedMark = terrain.taleaAtRO(lightTalea.position).modifiedCount
		//Move these down?
		lightTalea[LightTaleaData].lastTriggered = lastUpdatedMark
		lightTalea[LightTaleaData].lastUpdated = lastUpdatedMark

		lightTalea.setAll(0)

		//Left , Back , Bottom , Right , Front , Top
		val edgePairs = Cardinals.edgeCorners.map((tup) => (tup._1 * (Talea.dimension - 1), tup._2 * (Talea.dimension - 1)))


		val FullLight = lightData.globalFullLight(gli)
		for (((start, end), i) <- edgePairs.zipWithIndex.reverse) {
			val cardinal = Cardinals.cardinals(i)
			/* Only pull in values from generated taleae, there's no point in grabbing 0's */
			if (lightWindow.taleaAtRO(VoxelCoord(start) + cardinal)[LightTaleaData].lastUpdated >= 0) {
				for (x <- start.x to end.x optimized ; y <- start.y to end.y optimized ; z <- start.z to end.z optimized ) {
					val transp = transparency(terrainWindow(x, y, z))
					if (transp <= 0) {
						val adjLightValue = lightWindow(x + cardinal.x, y + cardinal.y, z + cardinal.z)
						val newLightValue = if (adjLightValue == FullLight && i == oppositePrimaryQ) {
							FullLight + transp
						} else {
							adjLightValue - 1 + transp
						}
						if (newLightValue > 0) {
							val cur = lightWindow(x, y, z)
							if (newLightValue > cur) {
								lightWindow(x, y, z) = newLightValue.toByte
								if (newLightValue != FullLight) {
									lightNodeQ.enqueue(x, y, z, oppositeSideIndices(i), newLightValue)
								} else {
									fullLightQ.enqueue(x, y, z, oppositeSideIndices(i), newLightValue)
								}
							}
						}
					}
				}
			}
		}

		initialFlood(lightTaleaPos, primaryQ, gli, lightNodeQ, fullLightQ)
		Noto.debug("Update finished")
	}

	def initialFlood(lightTaleaPos : VoxelCoord, primaryQ : Int, gli : Int,
						  lightNodeQ: FastLightNodeQueue,
						  fullLightQ: FastLightNodeQueue) {
		val lightWindow = VoxelWindow.centeredOnTalea(lightData.global(gli),lightTaleaPos)
		val terrainWindow = VoxelWindow.centeredOnTalea(terrainData.material, lightTaleaPos)
		val FullLight = lightData.globalFullLight(gli)
		val mainAxis = primaryQ % 3 // x for left, y for back, z for bottom, etc
		val mainAxisMinimum = if (lightWindow.taleaAtRO(VoxelCoord(TaleaLengthCardinals(primaryQ)))[LightTaleaData].lastUpdated >= 0) {
				0
			} else {
				-Talea.dimension
			}

		val av = MutableVoxelCoord(0,0,0)

		val retL = Array[Byte](0,0,0,0,0,0,0)
		val retT = Array[Byte](0,0,0,0,0,0,0)

		val centerLight: TTalea[Byte] = lightWindow.taleaAt(0,0,0)
		val centerTerrain: TTalea[Short] = terrainWindow.taleaAtRO(0,0,0)
		while (lightNodeQ.nonEmpty || fullLightQ.nonEmpty) {
			//			maxQSize = math.max(math.max(lightNodeQ.size,maxQSize),fullLightQ.size)
			val Q = if (fullLightQ.nonEmpty) {
				fullLightQ
			} else {
				lightNodeQ
			}
			Q.dequeue()
			val lightNode_x = Q.x
			val lightNode_y = Q.y
			val lightNode_z = Q.z
			val lightNode_cameFrom = Q.cameFrom
			val lightNode_lightValue = Q.lightValue

			if (lightNode_x > 0 && lightNode_y > 0 && lightNode_z > 0 && lightNode_x < Talea.dimension - 1 && lightNode_y < Talea.dimension - 1 && lightNode_z < Talea.dimension - 1) {
				val lv = centerLight(lightNode_x, lightNode_y, lightNode_z)

				if (lv == lightNode_lightValue) {
					//if this has been supplanted with a higher value, we no longer care
					if (lv > 1) {
						for (q <- 0 until 6 optimized) {
							if (q != oppositeSideIndices(lightNode_cameFrom)) {
								val ax = lightNode_x + cardinals(q).x
								val ay = lightNode_y + cardinals(q).y
								val az = lightNode_z + cardinals(q).z

								val transp = transparency(centerTerrain(ax, ay, az))
								if (transp <= 0) {
									val nlv = if (q == primaryQ && lv == FullLight) {
										FullLight + transp
									} else {
										lv - 1 + transp
									}

									if (centerLight(ax, ay, az) < nlv) {
										//										centerLight(ax, ay, az) = nlv.toByte
										centerLight(ax,ay,az) = nlv.toByte
										if (nlv != FullLight) {
											lightNodeQ.enqueue(ax, ay, az, q, nlv)
										} else {
											fullLightQ.enqueue(ax, ay, az, q, nlv)
										}
									}
								} else {
									centerLight(ax, ay, az) = -1.toByte
								}
							}
						}
					}
				}
			} else {
				val lv = lightWindow(lightNode_x, lightNode_y, lightNode_z)
				if (lv == lightNode_lightValue) {
					//if this has been supplanted with a higher value, we no longer care
					if (lv > 1) {
						for (q <- 0 until 6 optimized) {
							if (q != oppositeSideIndices(lightNode_cameFrom)) {
								av.x = lightNode_x + cardinals(q).x
								av.y = lightNode_y + cardinals(q).y
								av.z = lightNode_z + cardinals(q).z

								val containingTalea = lightWindow.taleaAt(av)
								if (av(mainAxis) >= mainAxisMinimum && containingTalea != null && containingTalea[LightTaleaData].lastUpdated >= 0) {
									val transp = transparency(terrainWindow(av.x,av.y,av.z))

									if (transp <= 0) {
										val nlv = if (q == primaryQ && lv == FullLight) {
											FullLight + transp
										} else {
											lv - 1 + transp
										}

										val cur = lightWindow(av)
										if (cur < nlv) {
//										if (lightWindow.setIfGreater(av.x,av.y,av.z,nlv.toByte) < nlv) {
											lightWindow(av) = nlv.toByte
											if (nlv != FullLight) {
												lightNodeQ.enqueue(av.x,av.y,av.z, q, nlv)
											} else {
												fullLightQ.enqueue(av.x,av.y,av.z, q, nlv)
											}
										}
									} else {
										lightWindow(av.x,av.y,av.z) = -1.toByte
									}
								}
							}
						}
					}
				}

			}
		}
	}

	def blackout ( lightTaleaPos : VoxelCoord, primaryQ : Int, gli : Int,
						lightNodeQ : mutable.Queue[LightNode],
						relightQ : mutable.Queue[Vec3i],
						fullLight : Byte )
	{
		val lightGrid = lightData.global(gli)
		val lightWindow = VoxelWindow.centeredOnTalea(lightGrid,lightTaleaPos)
		val mainAxis = primaryQ % 3
		val lightView = lightWindow

		while ( lightNodeQ.nonEmpty ) {
			val node = lightNodeQ.dequeue()
			val mainAxisP = node.pos(mainAxis)
			if ( mainAxisP >= absoluteMinimumZ && mainAxisP <= absoluteMaximumZ ) {

				var propagationCount = 0

				if ( node.lightValue > 1 ) {
					for (q <- 0 until 6 optimized) {
						if ( q != oppositeSideIndices(node.cameFrom) ){
							val x = node.x + cardinals(q).x
							val y = node.y + cardinals(q).y
							val z = node.z + cardinals(q).z

							val propagatedLightValue = (if (q == primaryQ && node.lightValue == fullLight) { fullLight } else { (node.lightValue - 1).toByte })// + transp
							val curValue = lightView.setIfEqual(x,y,z,propagatedLightValue.toByte,0.toByte)

							if ( curValue == propagatedLightValue ) {
								lightNodeQ.enqueue( LightNode(x,y,z,q,curValue) )

								propagationCount += 1
							} else if ( curValue == -1 || curValue == 0 ) { //count as a propagation, if everything around it is -1, we don't really care about it as a progenitor
								propagationCount += 1
							}
						}
					}
				}

				//this is our way of recognizing the original vox, we don't want to relight it
				if ( propagationCount < 6 && node.cameFrom != Cardinals.Center ) {
					relightQ.enqueue( Vec3i(node.x,node.y,node.z) )
				}
			}
		}
	}

	def relight (lightTaleaPos : VoxelCoord, primaryQ : Int, gli : Int,
					 relightQ : mutable.Queue[Vec3i],
					 lightNodeQ : mutable.Queue[LightNode],
					 fullLight : Byte )
	{
		val lightGrid = lightData.global(gli)
		val lightWindow = VoxelWindow.centeredOnTalea(lightGrid,lightTaleaPos)
		val terrainWindow = VoxelWindow.centeredOnTalea(terrainData.material,lightTaleaPos)

		val mainAxis = primaryQ % 3
		val oppositeQ = Cardinals.oppositeDirection(primaryQ)
		val terrainView = terrainWindow
		val lightView = lightWindow

		while ( relightQ.nonEmpty ) {
			val node = relightQ.dequeue()
			val mainAxisP = node(mainAxis)
			if ( mainAxisP >= absoluteMinimumZ && mainAxisP <= absoluteMaximumZ ) {
				var maxValue = 0
				for (q <- 0 until 6 optimized) {
					val x = node.x + cardinals(q).x
					val y = node.y + cardinals(q).y
					val z = node.z + cardinals(q).z
					val transp = transparency(terrainView(x,y,z))
					if (transp == 0) {
						val curValue = lightView(x,y,z)
						posit( q != primaryQ || curValue != fullLight || (x >= 0 && x < Talea.dimension && y >= 0 && y < Talea.dimension), "Max light from below : " + x + "," + y + "," + z)
						maxValue = scala.math.max(if (curValue == fullLight && q == oppositeQ) { curValue + transp } else { curValue - 1 + transp },maxValue)
					}
				}

				if ( maxValue > 0 ) {
					posit ( maxValue != fullLight || (node.x >= 0 && node.x < Talea.dimension && node.y >= 0 && node.y < Talea.dimension) , "EXT FL" )

					posit( fullLight < 0 || maxValue <= (fullLight -
						(if ( node.x < 0 ) { -node.x } else if ( node.x >= Talea.dimension ) { node.x - Talea.dimension } else { 0 }) -
						(if ( node.y < 0 ) { -node.y } else if ( node.y >= Talea.dimension ) { node.y - Talea.dimension } else { 0 })) , "Unreasonable max : " + node + " lv : " + maxValue )

					lightView(node.x,node.y,node.z) = maxValue.toByte
					lightNodeQ.enqueue( LightNode(node.x,node.y,node.z,Cardinals.Center,maxValue) )
					//Instead of Cardinals.Center, we could record the maxQ and use that, since it won't go back on itself
				}
			}
		}
	}

	def flood (lightTaleaPos : VoxelCoord, primaryQ : Int, gli : Int,
				  lightNodeQ : mutable.Queue[LightNode],
				  fullLight : Byte )
	{
		val lightGrid = lightData.global(gli)
		val lightWindow = VoxelWindow.centeredOnTalea(lightGrid,lightTaleaPos)
		val terrainGrid = terrainData.material
		val terrainWindow = VoxelWindow.centeredOnTalea(terrainGrid,lightTaleaPos)

		val mainAxis = primaryQ % 3
		val lightView = lightWindow
		val terrainView = terrainWindow
		val center = lightWindow.taleaAt(0,0,0).position
		while ( lightNodeQ.nonEmpty ) {
			val lightNode = lightNodeQ.dequeue()
			val mainAxisP = lightNode.pos(mainAxis)
			if ( mainAxisP >= absoluteMinimumZ && mainAxisP <= absoluteMaximumZ ) {
				val lv = lightView(lightNode.x,lightNode.y,lightNode.z)
				val propagatedValue = lv - 1
				val propagatedUnattenuatedValue = if ( lv == fullLight ) { fullLight } else { propagatedValue }
				if ( propagatedValue > 0 ) {
					for (q <- 0 until 6 optimized) {
						if ( q != oppositeSideIndices(lightNode.cameFrom) ) {
							val x = lightNode.x + cardinals(q).x
							val y = lightNode.y + cardinals(q).y
							val z = lightNode.z + cardinals(q).z

							val curVoxel = terrainView(x,y,z)
							val transp = transparency( curVoxel )

							val nlv = if ( q == primaryQ ) { propagatedUnattenuatedValue + transp } else { propagatedValue + transp }
							val curValue = lightView(x,y,z)
							if ( curValue < nlv && curValue != -1 ) {

								if ( transp <= 0 ) {
									lightView(x,y,z) = nlv.toByte
									lightNodeQ.enqueue( LightNode(x,y,z,q,nlv) )
								} else {
									lightView(x,y,z) = -1.toByte
								}
							}
						}
					}
				}
			}
		}
	}

	def updateLightingForTalea( lightTaleaPos : VoxelCoord,
										 primaryQ : Int, gli : Int,
										 localLighting : Boolean = false ) : Set[VoxelCoord] = {
		val terrain = terrainData.material
		val lightGrid = lightData.global(gli)
		val lightTalea = lightGrid.taleaAt(lightTaleaPos)
		val lastUpdatedAt = lightTalea[LightTaleaData].lastUpdated
		val lightWindow = VoxelWindow.centeredOnTalea(lightGrid, lightTaleaPos)
		val terrainWindow = VoxelWindow.centeredOnTalea(terrain, lightTaleaPos)
		val blockRev = terrainWindow.taleaAt(0,0,0).modifiedCount

		Noto.debug("Updating light for talea")

		val relightQ = TLrelightQ.get()
		val lightNodeQ = TLlightNodeQ.get()
		val currentFullLight = if ( ! localLighting ) { lightData.globalFullLight(gli).toByte } else { -2.toByte }

		val recentModifications = terrainWindow.taleaAtRO(0,0,0).loggedModifications.takeWhile( _.revision > lastUpdatedAt )
		Noto.debug("Bulking " + recentModifications.size + " mods")

//		lightWindow.taleae.filter(_ != null).foreach( _.lockModifiedCount() )
		val underBottomTalea = lightGrid.taleaAt(lightTalea.position + cardinals(primaryQ) * (Talea.dimension * 3))
		val subUnderBottomTalea = lightGrid.taleaAt(lightTalea.position + cardinals(primaryQ) * (Talea.dimension * 4))
//		lightGrid.deferredModificationBlock( underBottomTalea :: subUnderBottomTalea :: lightWindow.taleae.toList ){

			for ( mod <- recentModifications ; if ( transparency(mod.oldValue) <= 0 && transparency(mod.newValue) > 0 )) {
				Noto.finest("Successfully identified transparent to non transparent transition at mod: " + mod)
				lightNodeQ.enqueue( LightNode( mod.localX , mod.localY , mod.localZ , Cardinals.Center , lightWindow(mod.localX,mod.localY,mod.localZ) ) )
				lightWindow(mod.localX,mod.localY,mod.localZ) = -1.toByte
			}

			for ( mod <- recentModifications ; if ( transparency(mod.oldValue) == 0 && transparency(mod.newValue) < 0 )) {
				Noto.finest("Successfully identified transparent to partially transparent transition at mod: " + mod)
				lightNodeQ.enqueue( LightNode( mod.localX , mod.localY , mod.localZ , Cardinals.Center , lightWindow(mod.localX,mod.localY,mod.localZ) ) )
				lightWindow(mod.localX,mod.localY,mod.localZ) = (lightWindow(mod.localX,mod.localY,mod.localZ) + transparency(mod.newValue)).toByte
			}

			blackout(lightTaleaPos, primaryQ, gli,lightNodeQ, relightQ,currentFullLight)

			relight(lightTaleaPos,primaryQ, gli,relightQ,lightNodeQ,currentFullLight)

			flood(lightTaleaPos, primaryQ, gli,lightNodeQ,currentFullLight)

			for ( mod <- recentModifications ; if ( transparency(mod.oldValue) > 0 && transparency(mod.newValue) <= 0 )) {
				Noto.finest("Successfully identified non transparent to transparent transition at mod: " + mod)

				val maxValue = highestAdjacentLightValue(mod.localX,mod.localY,mod.localZ,gli,currentFullLight)
				lightWindow(mod.localX,mod.localY,mod.localZ) = maxValue.toByte
				if ( maxValue > 1 ) {
					lightNodeQ.enqueue( LightNode(mod.localX,mod.localY,mod.localZ,Cardinals.Center,maxValue) )
				}
			}

			for ( mod <- recentModifications ; if ( transparency(mod.oldValue) < 0 && transparency(mod.newValue) == 0 )) {
				Noto.finest("Successfully identified partially transparent to transparent transition at mod: " + mod)

				val maxValue = highestAdjacentLightValue(mod.localX,mod.localY,mod.localZ,gli,currentFullLight)
				//lightWindow(mod.localX,mod.localY,mod.localZ) - transparency(mod.oldValue)
				lightWindow(mod.localX,mod.localY,mod.localZ) = maxValue.toByte
				lightNodeQ.enqueue( LightNode(mod.localX,mod.localY,mod.localZ,Cardinals.Center,maxValue) )
			}

			flood(lightTaleaPos,primaryQ, gli,lightNodeQ,currentFullLight)

//			lightWindow.taleae.filter(_!=null).foreach( _.unlockModifiedCount() )
			lightTalea[LightTaleaData].lastUpdated = blockRev
//		}

		Set()//TODO: Actuall set of modified values
	}

	//		println("Max Q Size : " +maxQSize)
}