package arx.axistential.game.components.fluid

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/11/13
 * Time: 2:17 PM
 */

import arx.axistential.game.components.fluid.FinitePushFluidComputor.InfiniteFluidSourceData
import arx.axistential.game.data.world.FluidData
import arx.axistential.game.data.world.InfiniteFluidNode
import arx.axistential.game.data.world.InfiniteFluidSource
import arx.axistential.game.data.world.TerrainData
import arx.core.FibonacciHeap
import arx.core.datastructures.ProbabilisticVoxelSet
import arx.core.datastructures.TBareBonesSet
import arx.core.datastructures.VoxelBloomFilter
import arx.core.vec.Cardinals
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.ByteTalea
import arx.tmp.game.logic.datastructures.GenericTalea
import arx.tmp.game.logic.datastructures.TTaleaGrid
import arx.tmp.game.logic.entities.data.TGameEntityAuxData
import arx.tmp.game.logic.world.SpatialRegion
import arx.tmp.game.logic.world.data.VoxelWorldData
import com.carrotsearch.hppc.LongOpenHashSet

import scala.collection.mutable.ListBuffer
import scalaxy.loops._

class FinitePushFluidComputor extends TFluidComputor {
	import Cardinals._
	val baseCardinals = Cardinals.cardinals
	var pushCardinals = Array( baseCardinals(Bottom) , baseCardinals(Left) , baseCardinals(Right) , baseCardinals(Front) , baseCardinals(Back) )
	var infPushCardinals = Array( baseCardinals(Bottom) , baseCardinals(Left) , baseCardinals(Right) , baseCardinals(Front) , baseCardinals(Back) , baseCardinals(Top) )
	var pressureCardinals = Array( baseCardinals(Top) , baseCardinals(Bottom) , baseCardinals(Left) , baseCardinals(Right) , baseCardinals(Front) , baseCardinals(Back) )


	def settleInitialSimulation(world: World): Unit = {
		val sources = world.aux[FluidData].infiniteFluidSources
//		var break = false
//		while ( ! break ) {
//			break = true
			for ( source <- sources ) {
				source.auxDataOpt[InfiniteFluidSourceData] match {
					case Some(d) =>
					case None => {
						val data = source.aux[InfiniteFluidSourceData]
						data.spreadQ.enqueue(InfiniteFluidNode(source.location.x,source.location.y,source.location.z,0,7))
					}
				}
//				var q = 0; while ( q < 10 ) {
					updateInfiniteFluidSource(world,source,loop = true)
//					q += 1
//				}
//				break &&= (source.hasOutlet || ! source.spreadQ.nonEmpty)
			}
//		}
	}

	/** Advance the fluid simulation forward one step
	  *
	  * @param env the environment to update
	  * @param dirtyVoxels which voxels are considered "dirty" and in need of update
	  * @return list of voxels which should be considered "dirty" for the next timestep
	  */
	def stepFluidSimulation(env: World, dirtyVoxels: TBareBonesSet[VoxelCoord]) : TBareBonesSet[VoxelCoord] = {
		val fluidData = env.auxData[FluidData]
		fluidData.infiniteFluidSources.foreach( src => updateInfiniteFluidSource(env,src,loop = false) )
		
		if ( dirtyVoxels.isEmpty ) { return dirtyVoxels }
		val minimumZ = env.aux[VoxelWorldData].worldRegion.minZ + 100
		
		val newDirty = new ProbabilisticVoxelSet(math.max(dirtyVoxels.size * 1.5,256).toInt,0.001f)

		val fluidLevel = fluidData.fluidLevel
		val fluidType = fluidData.fluidType
		val terrain = env.aux[TerrainData].materialGrid
		val pressure = fluidData.direction

		val flowRate = fluidData.maxFluidLevel / 5
		val maxLevel = fluidData.maxFluidLevel
		val region = SpatialRegion.fromPoints(dirtyVoxels)

		val dirtyLists = Array.ofDim[ListBuffer[VoxelCoord]](region.dimensions.z+1)
		for ( vox <- dirtyVoxels ) {
			val cur = dirtyLists(vox.z - region.lowerCorner.z)
			if ( cur == null ) {
				val tmp = new ListBuffer[VoxelCoord]()
				tmp += vox
				dirtyLists(vox.z - region.lowerCorner.z) = tmp
			} else {
				dirtyLists(vox.z - region.lowerCorner.z) += vox
			}
		}

		fluidLevel.modificationBlock(region) {
			//			val sortedDirty = dirtyVoxels.values.toList.sortBy(_.z)
			//			for ( dirtyVoxel <- sortedDirty ) {
			for ( dirtyList <- dirtyLists ) {
				if ( dirtyList != null ) {
					for (  dirtyVoxel <- dirtyList ) {
						val originalWaterLevel : Int = fluidLevel(dirtyVoxel)
						if ( originalWaterLevel > 0 ) {
							var currentWaterLevel = originalWaterLevel
							var adjacentTerrainFlags = 0x000000
							var q = 0; while ( q < 5 ) { //Topless, we do not push up, that is accomplished separately
								val ax = dirtyVoxel.x + pushCardinals(q).x
								val ay = dirtyVoxel.y + pushCardinals(q).y
								val az = dirtyVoxel.z + pushCardinals(q).z

								if ( TerrainByteUtils.isFluidPassable(terrain(ax,ay,az)) ) {
									adjacentTerrainFlags |= (1 << q)
									val adjacentWaterLevel : Int = fluidLevel(ax,ay,az)
									if ( adjacentWaterLevel < maxLevel ) {
										val flow = if ( q == 0 ) {
											math.min(math.min(flowRate,maxLevel - adjacentWaterLevel),currentWaterLevel)
										} else {
											val baseFlow = math.min(math.min(flowRate,(currentWaterLevel - adjacentWaterLevel) >> 1),currentWaterLevel)
											//See (A) Below
											baseFlow
											//											if ( baseFlow == 0 && adjacentWaterLevel == 0 && currentWaterLevel > 0 ) {
											//												if ( TerrainByteUtils.isFluidPassable(terrain(ax,ay,az-1)) ) {
											//													1
											//												} else { baseFlow }
											//											} else { baseFlow }
										}

										if ( flow > 0 ) {
											currentWaterLevel -= flow
											if ( az > minimumZ ) {
												if ( adjacentWaterLevel == 0 ) {
													fluidType(ax,ay,az) = fluidType(dirtyVoxel)
												}
												fluidLevel.setUnsafe(ax,ay,az,(adjacentWaterLevel + flow).toShort)
											}
										}
									}
								}
								q += 1}
							if ( currentWaterLevel != originalWaterLevel || currentWaterLevel == 1 ){
								if ( currentWaterLevel > 0 ) {
									newDirty.add(dirtyVoxel)
								}
								if ( currentWaterLevel == 1 ) { currentWaterLevel = 0 }
								fluidLevel.setUnsafe(dirtyVoxel.x,dirtyVoxel.y,dirtyVoxel.z,currentWaterLevel.toShort)
								var q = 0; while ( q < 5 ) {
									if ( (adjacentTerrainFlags & (1 << q)) != 0 ) {
										val ax = dirtyVoxel.x + pushCardinals(q).x
										val ay = dirtyVoxel.y + pushCardinals(q).y
										val az = dirtyVoxel.z + pushCardinals(q).z

										newDirty.add(ax,ay,az)
									}
									q += 1}
								newDirty.add(dirtyVoxel.x,dirtyVoxel.y,dirtyVoxel.z+1)
							} else if ( pressureEnabled ) {
								computePressure(dirtyVoxel,newDirty,fluidData,terrain,fluidLevel,pressure)
							}
//						} else if ( originalWaterLevel < 0 ) { //we're assuming that this means infinite
//							var q = 0; while ( q < 5 ) {
//								val ax = dirtyVoxel.x + pushCardinals(q).x
//								val ay = dirtyVoxel.y + pushCardinals(q).y
//								val az = dirtyVoxel.z + pushCardinals(q).z
//
//								if ( TerrainByteUtils.isFluidPassable(terrain(ax,ay,az)) && az > minimumZ ) {
//									if ( q == 0 ) { q = 100000 }
//									val adjacentWaterLevel : Int = fluidLevel(ax,ay,az)
//									if ( adjacentWaterLevel >= 0 ) {
//										fluidType(ax,ay,az) = fluidType(dirtyVoxel)
//										fluidLevel.setUnsafe(ax,ay,az,originalWaterLevel.toShort)
//										newDirty.add(ax,ay,az)
//									}
//								}
//								q += 1
//							}
						}
					}
				}
			}
		}

		newDirty
	}
	
	def updateInfiniteFluidSource ( world : World, source : InfiniteFluidSource , loop : Boolean ) {
		val data = source.aux[InfiniteFluidSourceData]
		val spreadQ = data.spreadQ

		val minimumZ = world.worldRegionAsSpatialRegion.minZ + 100
		val fluidData = world.auxData[FluidData]

		val fluidLevel = fluidData.fluidLevel
		val terrain = world.aux[TerrainData].materialGrid

		val typeByte = world.aux[TerrainData].materialMapping(source.fluidType)

		var cont = true
		while ( cont ) {
			if ( ! loop ) { cont = false }

			if ( ! source.hasOutlet && spreadQ.nonEmpty ) {

				val head = spreadQ.dequeue()
				if ( head.z <= minimumZ ) {
					source.hasOutlet = true
				} else {
					fluidLevel.modificationBlock(VoxelCoord(head.x,head.y,head.z)) {
						fluidData.setInfiniteFluidLevel(head.x,head.y,head.z,fluidData.maxFluidLevel,typeByte)

						for ( q <- 0 until infPushCardinals.length optimized ) {
							val qprime = (q + head.from) % infPushCardinals.length
							val ax = head.x + infPushCardinals(qprime).x
							val ay = head.y + infPushCardinals(qprime).y
							val az = head.z + infPushCardinals(qprime).z

							if ( fluidData.effectiveFluidLevel(ax,ay,az) < fluidData.maxFluidLevel && TerrainByteUtils.isFluidPassable(terrain(ax,ay,az)) ){
								if ( infPushCardinals(qprime).z == 0 ) {
//									fluidData.setInfiniteFluidLevel(ax,ay,az,10000,typeByte)
								}

								if ( ! data.isClosed(ax,ay,az) ) {
									data.closeVoxel(ax,ay,az)
									val gcost = if(qprime == head.from) { 1.0f } else { 1.0f }
									spreadQ.enqueue( InfiniteFluidNode(ax,ay,az,head.g+gcost,qprime) )
								}
							}
						}
					}
				}
			} else {
				cont = false
			}
		}
	}

	val Q = new FloodFillQueue
	def computePressure(coord: VoxelCoord, newDirty : TBareBonesSet[VoxelCoord], fluidData : FluidData , terrain : TTaleaGrid[Byte,_] , fluidLevel: TTaleaGrid[Short, GenericTalea[Short]], pressure: TTaleaGrid[Byte, ByteTalea]) {
		val maxFlow = fluidData.maxFluidLevel / 8
		val fullThreshold : Short = (fluidData.maxFluidLevel * (7.0/8.0)).toShort
		if ( pressure(coord.x,coord.y,coord.z) != -1 ) {
			//			if ( fluidLevel(coord.x,coord.y,coord.z-1) >= fullThreshold ){
			val checked = new VoxelBloomFilter(500,0.001f)
			if ( TerrainByteUtils.isFluidPassable(terrain(coord.x,coord.y,coord.z-1)) ) {
				Q.enqueue(coord.x,coord.y,coord.z-1,0)
			}
			val curLevel = fluidLevel(coord)

			var found : VoxelCoord = null
			while ( Q.nonEmpty && (found == null || found.z >= coord.z) ) {
				Q.dequeue()
				val x = Q.x
				val y = Q.y
				val z = Q.z

				val fl = fluidLevel(x,y,z)
				if ( z < coord.z && fl < fluidData.maxFluidLevel ) {
					found = VoxelCoord(x,y,z)
				} else if ( z <= coord.z && fl + maxFlow < curLevel ) {
					found = VoxelCoord(x,y,z)
				} else if ( z <= coord.z && fl >= fullThreshold ) {
					var q = 0; while ( q < 6 ) {
						val ax = x + pressureCardinals(q).x
						val ay = y + pressureCardinals(q).y
						val az = z + pressureCardinals(q).z

						if ( checked.addIfAbsent(ax,ay,az) ) {
							if ( TerrainByteUtils.isFluidPassable(terrain(ax,ay,az)) ) {
								Q.enqueue(ax,ay,az,0)
							}
						}
						q += 1}
				}
			}
			Q.clear()

			if ( found == null ) {
				pressure(coord.x,coord.y,coord.z) = -1.toByte
			} else {
				val targetLevel = fluidLevel(found)

				//Currently the same regardless, could be changed to equalize in the case of
				val flow = if ( found.z >= coord.z ) {
					math.min(math.min((curLevel - targetLevel) >> 1,fluidData.maxFluidLevel - targetLevel),fluidData.maxFluidLevel/8)
				} else {
					math.min(math.min(curLevel,fluidData.maxFluidLevel - targetLevel),fluidData.maxFluidLevel/8)
				}

				if ( targetLevel == 0 ) {
					fluidData.fluidType(found) = fluidData.fluidType(coord)
				}
				fluidLevel(coord) = (curLevel - flow).toShort
				fluidLevel(found) = (targetLevel + flow).toShort
				newDirty.add(coord)
				newDirty.add(found)
			}
		}
		//		}
	}
	/*
	(A) This clause is to prevent the following from being a stable state

		 1	 0
		||| 0  0
		|||||||||

	 */


	/**
	 * Clear the pressure values to 0, flowing outward from all removed voxels, but not
	 * going downward below the minimum z of all removed voxels.
	 */
	override def voxelsRemoved(env: World, voxels: Traversable[VoxelCoord]) {
		if ( voxels.isEmpty ) { return }
		val fluidData = env.auxData[FluidData]
//		val terrain = env.terrain
		val fluidLevel = fluidData.fluidLevel
		val pressure = fluidData.direction

		val checked = new VoxelBloomFilter(500,0.001f)

		var minZ = Int.MaxValue
		voxels.foreach(v => {
			Q.enqueue(v.x,v.y,v.z,0)
			checked.add(v)
			minZ = math.min(minZ,v.z)
		})
		while ( Q.nonEmpty ) {
			Q.dequeue()
			val x = Q.x
			val y = Q.y
			val z = Q.z

			pressure.setUnsafe(x,y,z,0)

			for ( q <- 0 until 6 optimized ) {
				val ax = x + cardinalsX(q)
				val ay = y + cardinalsY(q)
				val az = z + cardinalsZ(q)
				if ( az >= minZ ) {
					if ( checked.addIfAbsent(ax,ay,az) ) {
						if ( fluidLevel(ax,ay,az) > 0 ) {
							Q.enqueue(ax,ay,az,0)
						}
					}
				}
			}
		}
	}

	override def isPull = false
}

object FinitePushFluidComputor {

	class InfiniteFluidSourceData extends TGameEntityAuxData {
		val spreadQ = new FibonacciHeap[InfiniteFluidNode]
		val closedSet = new LongOpenHashSet(1024)

		def isClosed ( x : Int ,y : Int , z : Int ) = closedSet.contains(VoxelCoord.hashL(x,y,z))
		def closeVoxel ( voxel : VoxelCoord ) { closedSet.add( VoxelCoord.hashL(voxel.x,voxel.y,voxel.z)) }
		def openVoxel ( voxel : VoxelCoord ) { closedSet.remove( VoxelCoord.hashL(voxel.x,voxel.y,voxel.z)) }
		def closeVoxel ( x : Int , y : Int , z : Int ) { closedSet.add( VoxelCoord.hashL(x,y,z) ) }
		def openVoxel ( x : Int , y : Int , z : Int ) { closedSet.remove( VoxelCoord.hashL(x,y,z) ) }

//		val closedSet = new mutable.HashSet[VoxelCoord]
//
//		def isClosed ( x : Int ,y : Int , z : Int ) = closedSet.contains(VoxelCoord(x,y,z))
//		def closeVoxel ( voxel : VoxelCoord ) { closedSet.add( voxel ) }
//		def openVoxel ( voxel : VoxelCoord ) { closedSet.remove( voxel ) }
//		def closeVoxel ( x : Int , y : Int , z : Int ) { closedSet.add( VoxelCoord(x,y,z) ) }
//		def openVoxel ( x : Int , y : Int , z : Int ) { closedSet.remove( VoxelCoord(x,y,z) ) }
//

	}
}