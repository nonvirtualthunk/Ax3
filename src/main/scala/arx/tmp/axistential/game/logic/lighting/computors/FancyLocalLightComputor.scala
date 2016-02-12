package arx.axistential.game.logic.lighting.computors

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 12/13/12
 * Time: 11:33 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.data.world.TerrainData
import arx.core.datastructures.RingBuffer
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.TaleaGrid.TaleaModificationsCompletedEvent
import arx.tmp.game.logic.datastructures.ExtendedGenericTaleaGridWindow
import arx.tmp.game.logic.datastructures.ITalea
import arx.tmp.game.logic.datastructures.Talea
import arx.tmp.game.logic.datastructures.TaleaGrid
import arx.tmp.game.logic.entities.TLightSource
import arx.tmp.game.logic.lighting.OctantShadowGrid
import arx.tmp.game.logic.lighting.PerLightData
import arx.tmp.game.logic.world.data.LightData

object FancyLocalLightComputor extends TLocalLightComputor {
	val ringBufferCache = new ThreadLocal[RingBuffer[Int]] {
		override def initialValue() = new RingBuffer[Int]
	}
	val relightBufferCache = new ThreadLocal[RingBuffer[Int]] {
		override def initialValue() = new RingBuffer[Int]
	}

	val ObstructionShadowLevel = 0.9f

	def provideRingBuffer = {
		val r = ringBufferCache.get
		r.clear()
		r
	}

	def provideRelightBuffer = {
		val r = relightBufferCache.get
		r.clear()
		r
	}

	def canHandleLightSource(ls: TLightSource) = true

	def addLightSource(env: World, lightSource: TLightSource) {
		val Q = provideRingBuffer
		Q.enqueue(0)
		Q.enqueue(0)
		Q.enqueue(0)

		val mods = computeLight(env,lightSource,Q,overwrite = false)
		env.aux[LightData].globalLighting(0).fireEvent( TaleaModificationsCompletedEvent(mods) )
	}


	def computeLight (env : World, lightSource : TLightSource, Q : RingBuffer[Int], overwrite : Boolean ) = {
		val startTime = System.nanoTime

		val perLightData = env.auxData[PerLightData]
		val shadowGrid = perLightData.shadowGridFor(lightSource)
		val lightPos = lightSource.lightLocation.toVoxelCoord
		val terrainWindow = env.aux[TerrainData].materialGrid.extendedWindowCenteredOnTaleaAt(lightPos,readOnly = true)
		val lightGrid = env.aux[LightData].lightGridForLightSource(lightSource)
		val lightWindow = lightGrid.extendedWindowCenteredOnTaleaAt(lightPos,readOnly = false)
		val lightStrength = lightSource.lightStrength.toFloat

		val windowOffset = lightPos - terrainWindow.center
		val wx = windowOffset.x
		val wy = windowOffset.y
		val wz = windowOffset.z

		val mods = lightGrid.deferredModificationBlock(lightWindow) {
			while ( Q.nonEmpty ) {
				//Coordinates relative to the light's position
				val dx = Q.dequeue()
				val dy = Q.dequeue()
				val dz = Q.dequeue()

				val distance = sqrtf(dx*dx+dy*dy+dz*dz)
				if ( distance < lightStrength ) {
					if ( dx == 0 && dy == 0 && dz == 0 ) {
						lightWindow(wx,wy,wz) = lightStrength.toByte
						shadowGrid(0,0,0) = 0.0f
					} //do nothing, origin
					else {
						val terrainByte = terrainWindow(wx + dx,wy + dy,wz + dz)
						if ( ! TerrainByteUtils.isTransparent(terrainByte) ) {
							shadowGrid(dx,dy,dz) = ObstructionShadowLevel
							lightWindow(dx + wx,dy + wy,dz + wz) = 0.toByte
						} else {
							val absDx = math.abs(dx)
							val absDy = math.abs(dy)
							val absDz = math.abs(dz)

							val pcntX = absDx.toFloat / (absDx + absDy + absDz).toFloat
							val pcntY = absDy.toFloat / (absDx + absDy + absDz).toFloat
							val pcntZ = absDz.toFloat / (absDx + absDy + absDz).toFloat

							val signX = if ( dx > 0 ) { 1 } else { -1 }
							val signY = if ( dy > 0 ) { 1 } else { -1 }
							val signZ = if ( dz > 0 ) { 1 } else { -1 }

							val shadowPcnt = shadowGrid(dx-signX,dy,dz) * pcntX + shadowGrid(dx,dy-signY,dz) * pcntY + shadowGrid(dx,dy,dz-signZ) * pcntZ
							shadowGrid(dx,dy,dz) = shadowPcnt
							val attenuatedLight = (lightStrength - distance) * 2.0f
							val newLight = (attenuatedLight * (1.0f - (shadowPcnt ))).toByte

							if ( overwrite ) {
								lightWindow(dx + wx,dy + wy,dz + wz) = newLight
							} else {
//								lightWindow(dx + wx,dy + wy,dz + wz) = math.max(lightWindow(dx + wx,dy + wy,dz + wz),newLight).toByte
								lightWindow.setIfGreater(dx + wx,dy + wy,dz + wz,newLight)
							}
						}
					}

					addSuccessors(Q,dx,dy,dz)
				}
			}
		}

		val endtime = System.nanoTime
		Noto.finest("Time taken for light recomputation : " + ((endtime - startTime).toDouble / 1000000000.0) + "s")

		mods
	}
	//we probably don't need to clear the shadow buffer, but keep in mind in case things be weird

	def addSuccessors ( Q : RingBuffer[Int] , dx : Int , dy : Int , dz : Int ) {
		if ( dx == 0 && dy == 0 && dz == 0 ) { addV(Q,dx+1,dy,dz) ; addV(Q,dx,dy+1,dz) ; addV(Q,dx-1,dy,dz) ; addV(Q,dx,dy-1,dz) ; addV(Q,dx,dy,dz+1) ; addV(Q,dx,dy,dz-1) }
		else if ( dx == 0 && dy == 0 && dz < 0 ) { addV(Q,dx+1,dy,dz) ; addV(Q,dx,dy+1,dz) ; addV(Q,dx-1,dy,dz) ; addV(Q,dx,dy-1,dz) ; addV(Q,dx,dy,dz-1) }
		else if ( dx == 0 && dy == 0 && dz > 0 ) { addV(Q,dx+1,dy,dz) ; addV(Q,dx,dy+1,dz) ; addV(Q,dx-1,dy,dz) ; addV(Q,dx,dy-1,dz) ; addV(Q,dx,dy,dz+1) }
		else if ( dx > 0 && dy >= 0 ) { addV(Q,dx,dy+1,dz) }
		else if ( dx <= 0 && dy > 0 ) { addV(Q,dx-1,dy,dz) }
		else if ( dx < 0 && dy <= 0 ) { addV(Q,dx,dy-1,dz) }
		else if ( dx >= 0 && dy < 0 ) { addV(Q,dx+1,dy,dz) }

		if ( dx == 0 && dy > 0 ) { addV(Q,dx,dy+1,dz) }
		else if ( dx == 0 && dy < 0 ) { addV(Q,dx,dy-1,dz) }
		else if ( dx > 0 && dy == 0 ) { addV(Q,dx+1,dy,dz) }
		else if ( dx < 0 && dy == 0 ) { addV(Q,dx-1,dy,dz) }
	}

	@inline
	def addV ( Q : RingBuffer[Int] , x : Int , y : Int , z : Int ) {
		Q.enqueue(x)
		Q.enqueue(y)
		Q.enqueue(z)
	}

	def moveLightSource(env: World, lightSource: TLightSource, oldWorldPos: Vec3i, newWorldPos: Vec3i) {
		val lightPos = newWorldPos
		val lightStrength = lightSource.lightStrength.toFloat
		val lightStrengthi = lightSource.lightStrength
		val perLightData = env.auxData[PerLightData]
		val shadowGrid = perLightData.shadowGridFor(lightSource)
		shadowGrid match {
			case oct : OctantShadowGrid => oct.clear()
		}
		val lightChannel = env.aux[LightData].localLightChannelFor(lightSource)
		val lightGrid = lightChannel.grid
		val lightWindow = lightGrid.extendedWindowCenteredOnTaleaAt(VoxelCoord(lightPos),readOnly = false)

		val wx = newWorldPos.x - lightWindow.center.x
		val wy = newWorldPos.y - lightWindow.center.y
		val wz = newWorldPos.z - lightWindow.center.z

//		for ( dx <- -lightStrengthi-1 to lightStrengthi + 1 optimized;
//				dy <- -lightStrengthi-1 to lightStrengthi + 1 optimized;
//				dz <- -lightStrengthi-1 to lightStrengthi + 1 optimized)
//		{
//			lightWindow(wx+dx,wy+dy,wz+dz) = 0.toByte
//		}

		val Q = provideRingBuffer
		addV(Q,0,0,0)

		val startTime = System.nanoTime

		val terrainWindow = env.aux[TerrainData].materialGrid.extendedWindowCenteredOnTaleaAt(VoxelCoord(lightPos),readOnly = true)


		val mods = lightGrid.deferredModificationBlock(lightWindow) {
			while ( Q.nonEmpty ) {
				//Coordinates relative to the light's position
				val dx = Q.dequeue()
				val dy = Q.dequeue()
				val dz = Q.dequeue()

				val distance = sqrtf(dx*dx+dy*dy+dz*dz)
				if ( distance < lightStrength + 1 ) {
					if ( dx == 0 && dy == 0 && dz == 0 ) {
						lightWindow(wx,wy,wz) = lightStrength.toByte
						shadowGrid(0,0,0) = 0.0f
					} //do nothing, origin
					else {
						val terrainByte = terrainWindow(wx + dx,wy + dy,wz + dz)
						if ( ! TerrainByteUtils.isTransparent(terrainByte) ) {
							shadowGrid(dx,dy,dz) = ObstructionShadowLevel
							lightWindow(dx + wx,dy + wy,dz + wz) = 0.toByte
						} else {
							val absDx = math.abs(dx)
							val absDy = math.abs(dy)
							val absDz = math.abs(dz)

							val pcntX = absDx.toFloat / (absDx + absDy + absDz).toFloat
							val pcntY = absDy.toFloat / (absDx + absDy + absDz).toFloat
							val pcntZ = absDz.toFloat / (absDx + absDy + absDz).toFloat

							val signX = if ( dx > 0 ) { 1 } else { -1 }
							val signY = if ( dy > 0 ) { 1 } else { -1 }
							val signZ = if ( dz > 0 ) { 1 } else { -1 }

							val shadowPcnt = shadowGrid(dx-signX,dy,dz) * pcntX + shadowGrid(dx,dy-signY,dz) * pcntY + shadowGrid(dx,dy,dz-signZ) * pcntZ
							shadowGrid(dx,dy,dz) = shadowPcnt
							if ( shadowPcnt > 0.0f ) {
								shadowGrid(dx,dy,dz) = shadowPcnt
							}
							val attenuatedLight = (lightStrength - distance) * 2.0f
							val newLight = (attenuatedLight * (1.0f - shadowPcnt)).toByte

							lightWindow(dx + wx,dy + wy,dz + wz) = newLight
						}
					}

					addSuccessors(Q,dx,dy,dz)
				}
			}
		}

		val endTime = System.nanoTime
		Noto.info("Light movement time : " + (((endTime - startTime) / 1000000000.0)) + " seconds, newPos: " + newWorldPos)

		env.aux[LightData].globalLighting(0).fireEvent( TaleaModificationsCompletedEvent(mods) )



//		for ( axis <- 0 until 3 ) {
//			val axisDelta = newWorldPos(axis) - oldWorldPos(axis)
//			val absAxisDelta = axisDelta.abs
//			if ( absAxisDelta > 0 ) {
//				if ( absAxisDelta > 1 ) { Noto.warn("Fuck off...more than one movement at a time?") }
//				val delta = Vec3i(0,0,0)
//				delta(axis) = axisDelta
//
//				val s1 = s+1
//				val xstart = if ( delta(axis) > 0 ) { -s } else { s }
//				val xend = if ( delta(axis) > 0 ) { s1 } else { -s1 }
//				val dx = if ( xend > xstart ) { 1 } else { -1 }
//				val ystart = if ( delta(axis) > 0 ) { -s } else { s }
//				val yend = if ( delta(axis) > 0 ) { s1 } else { -s1 }
//				val dy = if ( yend > ystart ) { 1 } else { -1 }
//				val zstart = if ( delta(axis) > 0 ) { -s } else { s }
//				val zend = if ( delta(axis) > 0 ) { s1 } else { -s1 }
//				val dz = if ( zend > zstart ) { 1 } else { -1 }
//
//
//				var x = xstart;while ( x != xend ) {
//					var y = ystart;while ( y != yend ) {
//					 	var z = zstart;while ( z != zend ) {
//							if ( (axis == 0 && x == xstart) || (axis == 1 && y == ystart) || (axis == 2 && z == zstart) ) {
//								lightWindow(x+wx,y+wy,z+wz) = 0.toByte
//							}
//							shadowGrid(x,y,z) = shadowGrid(x+delta.x,y+delta.y,z+delta.z)
//						z += dz}
//					y += dy}
//				x += dx}
//			}
//		}


	}

	def removeLightSource(env: World, lightSource: TLightSource, lightLoc: VoxelCoord) {
		val perLightData = env.auxData[PerLightData]
		val shadowGrid = perLightData.shadowGridFor(lightSource)
		val channel = env.aux[LightData].localLightChannelFor(lightSource)
		perLightData.removeShadowGridFor(lightSource)
		val terrainWindow = env.aux[TerrainData].materialGrid.extendedWindowCenteredOnTaleaAt(lightLoc,readOnly=true)
		val lightWindow = channel.grid.extendedWindowCenteredOnTaleaAt(lightLoc,readOnly=false)

		val relightQ = provideRelightBuffer
		val Q = provideRingBuffer
		addV(Q,0,0,0)

		val ax = lightSource.lightLocation.x
		val ay = lightSource.lightLocation.y
		val az = lightSource.lightLocation.z

		val wx = ax - terrainWindow.center.x
		val wy = ay - terrainWindow.center.y
		val wz = az - terrainWindow.center.z

		val s = lightSource.lightStrength.toFloat

		val mods = channel.grid.deferredModificationBlock(lightWindow) {
			//If there are other lights remaining on this channel we have to be fancy, otherwise, we can just nuke the whole grid, more or less
			if ( channel.lightSources.size > 0 ) {
				while ( Q.nonEmpty ) {
					val dx = Q.dequeue()
					val dy = Q.dequeue()
					val dz = Q.dequeue()
					val dist = sqrtf(dx*dx+dy*dy+dz*dz)

					if ( dist < s ) {
						val curShadow = shadowGrid(dx,dy,dz)
						val curLightContribution = ((s - dist) * (1.0f - curShadow) * 2.0f).toInt
						val curLightValue = lightWindow(wx+dx,wy+dy,wz+dz)

						//Technically, should only be ==, but we want to account for slight inaccuracies, and a < is incorrect
						//and should be rectified anyway, so it works out
						if ( curLightValue <= curLightContribution ) {
							addV(relightQ,ax+dx,ay+dy,az+dz)
							lightWindow(wx+dx,wy+dy,wz+dz) = 0.toByte
						}

						addSuccessors(Q,dx,dy,dz)
					}
				}

				relight(env,channel.lightSources,lightWindow,relightQ,lightLoc)
			} else {
				lightWindow.taleae.foreach( _.setAll(0.toByte) )
			}
		}

		env.aux[LightData].globalLighting(0).fireEvent( TaleaModificationsCompletedEvent(mods) )
	}

	def computeLightAfterBlockout(env: World, lightSource: TLightSource, Q: RingBuffer[Int], relightQ: RingBuffer[Int], minDistance : Float, negX: Boolean, negY: Boolean, negZ: Boolean, posX: Boolean, posY: Boolean, posZ: Boolean) {
		val startTime = System.nanoTime

		val perLightData = env.auxData[PerLightData]
		val shadowGrid = perLightData.shadowGridFor(lightSource)
		val lightPos = lightSource.lightLocation.toVoxelCoord
		val terrainWindow = env.aux[TerrainData].materialGrid.extendedWindowCenteredOnTaleaAt(lightPos,readOnly = true)
		val lightGrid = env.aux[LightData].lightGridForLightSource(lightSource)
		val lightWindow = lightGrid.extendedWindowCenteredOnTaleaAt(lightPos,readOnly = false)
		val lightStrength = lightSource.lightStrength.toFloat

		val windowOffset = lightPos - terrainWindow.center
		val wx = windowOffset.x
		val wy = windowOffset.y
		val wz = windowOffset.z

		val ax = lightPos.x
		val ay = lightPos.y
		val az = lightPos.z


		while ( Q.nonEmpty ) {
			//Coordinates, relative to the light's position
			val dx = Q.dequeue()
			val dy = Q.dequeue()
			val dz = Q.dequeue()

			if ( ((dx <= 0 && negX) || (dx >= 0 && posX)) && ((dy <= 0 && negY) || (dy >= 0 && posY)) && ((dz <= 0 && negZ) || (dz >= 0 && posZ)) ) {
				val distance = sqrtf(dx*dx+dy*dy+dz*dz)
				if ( distance < lightStrength ) {
					if ( distance < minDistance || (dx == 0 && dy == 0 && dz == 0) ) {} //do nothing, origin, or closer than the shadowers
					else {
						val curShadow = shadowGrid(dx,dy,dz)
						if ( curShadow < 1.0f ) {
							val absDx = math.abs(dx)
							val absDy = math.abs(dy)
							val absDz = math.abs(dz)

							val pcntX = absDx.toFloat / (absDx + absDy + absDz).toFloat
							val pcntY = absDy.toFloat / (absDx + absDy + absDz).toFloat
							val pcntZ = absDz.toFloat / (absDx + absDy + absDz).toFloat

							val signX = if ( dx > 0 ) { 1 } else { -1 }
							val signY = if ( dy > 0 ) { 1 } else { -1 }
							val signZ = if ( dz > 0 ) { 1 } else { -1 }

							val shadowPcnt = shadowGrid(dx-signX,dy,dz) * pcntX + shadowGrid(dx,dy-signY,dz) * pcntY + shadowGrid(dx,dy,dz-signZ) * pcntZ
							if ( shadowPcnt > curShadow + 0.01f) {
								shadowGrid(dx,dy,dz) = shadowPcnt
								val newLight = ((lightStrength - distance) * (1.0f - shadowPcnt) * 2.0f).toByte
								lightWindow(dx + wx,dy + wy,dz + wz) = newLight
								addV(relightQ,dx+ax,dy+ay,dz+az)
							}
						}
					}

					addSuccessors(Q,dx,dy,dz)
				}
			}
		}

		val endtime = System.nanoTime
		Noto.finest("Time taken for blockout recomputation : " + ((endtime - startTime).toDouble / 1000000000.0) + "s")
	}

	def computeLightAfterBlockRemoval(env: World, lightSource: TLightSource, Q: RingBuffer[Int], minDistance : Float, negX: Boolean, negY: Boolean, negZ: Boolean, posX: Boolean, posY: Boolean, posZ: Boolean) {
		val startTime = System.nanoTime

		val perLightData = env.auxData[PerLightData]
		val shadowGrid = perLightData.shadowGridFor(lightSource)
		val lightPos = lightSource.lightLocation.toVoxelCoord
		val terrainWindow = env.aux[TerrainData].materialGrid.extendedWindowCenteredOnTaleaAt(lightPos,readOnly = true)
		val lightGrid = env.aux[LightData].lightGridForLightSource(lightSource)
		val lightWindow = lightGrid.extendedWindowCenteredOnTaleaAt(lightPos,readOnly = false)
		val lightStrength = lightSource.lightStrength.toFloat

		val windowOffset = lightPos - terrainWindow.center
		val wx = windowOffset.x
		val wy = windowOffset.y
		val wz = windowOffset.z

		val ax = lightPos.x
		val ay = lightPos.y
		val az = lightPos.z


		while ( Q.nonEmpty ) {
			//Coordinates, relative to the light's position
			val dx = Q.dequeue()
			val dy = Q.dequeue()
			val dz = Q.dequeue()

			if ( ((dx <= 0 && negX) || (dx >= 0 && posX)) && ((dy <= 0 && negY) || (dy >= 0 && posY)) && ((dz <= 0 && negZ) || (dz >= 0 && posZ)) ) {
				val distance = sqrtf(dx*dx+dy*dy+dz*dz)
				if ( distance < lightStrength ) {
					if ( distance < minDistance || (dx == 0 && dy == 0 && dz == 0) ) {} //do nothing, origin, or closer than the shadowers
					else {
						val terrainByte = terrainWindow(wx + dx,wy + dy,wz + dz)
						if ( ! TerrainByteUtils.isTransparent(terrainByte) ) {
							shadowGrid(dx,dy,dz) = ObstructionShadowLevel
							lightWindow(dx + wx,dy + wy,dz + wz) = 0.toByte
						} else {
							val absDx = math.abs(dx)
							val absDy = math.abs(dy)
							val absDz = math.abs(dz)

							val pcntX = absDx.toFloat / (absDx + absDy + absDz).toFloat
							val pcntY = absDy.toFloat / (absDx + absDy + absDz).toFloat
							val pcntZ = absDz.toFloat / (absDx + absDy + absDz).toFloat

							val signX = if ( dx > 0 ) { 1 } else { -1 }
							val signY = if ( dy > 0 ) { 1 } else { -1 }
							val signZ = if ( dz > 0 ) { 1 } else { -1 }

							val shadowPcnt = shadowGrid(dx-signX,dy,dz) * pcntX + shadowGrid(dx,dy-signY,dz) * pcntY + shadowGrid(dx,dy,dz-signZ) * pcntZ
							shadowGrid(dx,dy,dz) = shadowPcnt
							val newLight = ((lightStrength - distance) * (1.0f - (shadowPcnt )) * 2.0f).toByte

							lightWindow.setIfGreater(dx + wx,dy + wy,dz + wz,newLight)
						}
					}

					addSuccessors(Q,dx,dy,dz)
				}
			}
		}

		val endtime = System.nanoTime
		Noto.finest("Time taken for block removal recomputation : " + ((endtime - startTime).toDouble / 1000000000.0) + "s")
	}

	def updateLightingForTalea(env: World, lightChannel : LightData.LocalLightChannel, lightTalea: LightData.LightTaleaType) = {
		val lightGrid = lightChannel.grid
		val lastUpdatedAt = lightTalea.lastUpdated
		val lightWindow = lightGrid.extendedWindowCenteredOnTaleaAt( lightTalea.position , readOnly = false )
		val terrainWindow = env.aux[TerrainData].materialGrid.extendedWindowCenteredOnTaleaAt( lightTalea.position , readOnly = true )
		val blockRev = terrainWindow.centerTalea.modifiedCount
		val perLightData = env.auxData[PerLightData]

		val recentModifications = terrainWindow.centerTalea.loggedModifications.takeWhile( _.revision > lastUpdatedAt )
		var modifiedTaleae : Set[ITalea[_]] = Set()

		val taleaMidPos = lightTalea.position + (Talea.dimension>>1)

		val startTime = System.nanoTime

//		lightWindow.taleae.filter(_ != null).foreach( _.lockModifiedCount() )

		/* The idea here is that we only need to recompute the cone of voxels that could potentially
			be shadowed by an added voxel. Since this differs between light sources, we do this once
			per light source. The algorithm used is much like normal shadowcasting, with a few alterations,
			namely that it compares against existing shadow values, and only operates on voxels that become
			more shadowed. Second, it tracks all voxels it operates on. Afterwards, the merged list of all
			modified voxels is run through and the maximum contributions of all light sources are maxed
			together to find the final light value.
		 */
		modifiedTaleae ++= lightGrid.deferredModificationBlock( lightWindow.taleae.toList ){
			val relightBuffer = provideRelightBuffer
			for ( lightSource <- lightChannel.lightSources ) {
				val shadowGrid = perLightData.shadowGridFor(lightSource)
				val loc = lightSource.lightLocation
				val strength = lightSource.lightStrength
				val dx = math.abs(taleaMidPos.x - loc.x) - strength - (Talea.dimension>>1)
				val dy = math.abs(taleaMidPos.y - loc.y) - strength - (Talea.dimension>>1)
				val dz = math.abs(taleaMidPos.z - loc.z) - strength - (Talea.dimension>>1)

				if ( dx < 0 || dy < 0 || dz < 0 ) {
					val Q = provideRingBuffer

					var negX = false
					var negY = false
					var negZ = false
					var posX = false
					var posY = false
					var posZ = false
					var addition = false
					var removal = false
					var minDistance = 1000.0f

					for ( mod <- recentModifications ) {
						val fromTransparent = TerrainByteUtils.isTransparent(mod.oldValue)
						val toTransparent = TerrainByteUtils.isTransparent(mod.newValue)

						if ( fromTransparent != toTransparent ) {
							val absPos = lightTalea.position + mod.position
							val relX = absPos.x - loc.x
							val relY = absPos.y - loc.y
							val relZ = absPos.z - loc.z

							if ( relX <= 0 ) { negX = true }
							if ( relX >= 0 ) { posX = true }
							if ( relY <= 0 ) { negY = true }
							if ( relY >= 0 ) { posY = true }
							if ( relZ <= 0 ) { negZ = true }
							if ( relZ >= 0 ) { posZ = true }
							if ( relX.abs <= strength && relY.abs <= strength && relZ <= strength ) {
								minDistance = minDistance.min( sqrtf(relX*relX+relY*relY+relZ*relZ) )
								if ( fromTransparent && ! toTransparent ) {
									addition = true
									shadowGrid(relX,relY,relZ) = ObstructionShadowLevel
									lightGrid(absPos) = 0.toByte
								} else if ( ! fromTransparent && toTransparent ) {
									removal = true
								}
							}
						}
					}

					if ( addition ) {
						addV(Q,0,0,0)
						computeLightAfterBlockout(env,lightSource,Q,relightBuffer,minDistance,negX,negY,negZ,posX,posY,posZ)
					}
					if ( removal ) {
						addV(Q,0,0,0)
						computeLightAfterBlockRemoval(env,lightSource,Q,minDistance,negX,negY,negZ,posX,posY,posZ)
					}
				}
			}

			//If there was only one, then the previous step will already have populated the lighting correctly
			if ( lightChannel.lightSources.size > 1 ) {
				relight(env,lightChannel.lightSources,lightWindow,relightBuffer,VoxelCoord(taleaMidPos))
			}

		}

//		lightWindow.taleae.filter(_!=null).foreach( _.unlockModifiedCount() )
		lightTalea.lastUpdated = blockRev

		val endTime = System.nanoTime
		Noto.finest("Update Talea full time : " + ((endTime - startTime).toDouble / 1000000000.0) + "s")

		modifiedTaleae
	}

	def relight ( env : World, lightChannelLightSources : List[TLightSource] , lightWindow : ExtendedGenericTaleaGridWindow[Byte,LightData.LightTaleaType], relightBuffer : RingBuffer[Int] , centerPosition : VoxelCoord ) {
		if ( relightBuffer.nonEmpty ) {
			val perLightData = env.auxData[PerLightData]
			val taleaCornerVectorLength = lengthSafe( Vec3i((Talea.dimension>>1),(Talea.dimension>>1),(Talea.dimension>>1)) )
			val lightSourceArray = lightChannelLightSources.filter( l => lengthSafe(l.lightLocation - centerPosition) < l.lightStrength + taleaCornerVectorLength ).toArray
			val shadowGridArray = lightSourceArray.map ( perLightData.shadowGridFor ).toArray
			val lightPositions = lightSourceArray.map( _.lightLocation.toVoxelCoord ).toArray
			val lightStrengths = lightSourceArray.map( _.lightStrength.toInt ).toArray

			val wx = lightWindow.center.x
			val wy = lightWindow.center.y
			val wz = lightWindow.center.z

			while ( relightBuffer.nonEmpty ) {
				//Absolute x values, voxel coordinates
				val ax = relightBuffer.dequeue()
				val ay = relightBuffer.dequeue()
				val az = relightBuffer.dequeue()

				var maxLV = 0
				var li = 0; while ( li < lightSourceArray.length ) {
					val dx = ax - lightPositions(li).x
					val dy = ay - lightPositions(li).y
					val dz = az - lightPositions(li).z
					val s = lightStrengths(li)

					val dist = sqrtf(dx*dx+dy*dy+dz*dz)
					if ( dist <= s ) {
						val shadow = shadowGridArray(li)(dx,dy,dz)
						val lv = ((s - dist) * (1.0f - shadow) * 2.0f).toInt
						maxLV = math.max(lv,maxLV)
					}
				li += 1}

				lightWindow( ax - wx , ay - wy , az - wz ) = maxLV.toByte
			}
		}
	}

	val centerX = 32 * 5
	val centerY = 32 * 5
	val centerZ = 32 * 5
	val center = VoxelCoord(centerX,centerY,centerZ)
}







