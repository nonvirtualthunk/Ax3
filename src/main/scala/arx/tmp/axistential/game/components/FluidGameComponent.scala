package arx.axistential.game.components

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/11/13
 * Time: 2:16 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.game.components.fluid.FinitePushFluidComputor
import arx.axistential.game.data.world.FluidData
import arx.axistential.game.data.world.TerrainData
import arx.core.CachedBoolean
import arx.core.datastructures._
import arx.core.mathutil.RunningAverage
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.TaleaGrid.TaleaModificationsCompletedEvent
import arx.tmp.game.logic.datastructures.TLoggingTalea
import arx.tmp.game.logic.datastructures.TaleaGrid

import scala.collection.GenTraversableOnce
import scala.collection.mutable.ArrayBuffer

class FluidGameComponent extends GameEngineComponent {
	val computor = new FinitePushFluidComputor

	var dirtyVoxels : TBareBonesSet[VoxelCoord] = new ProbabilisticVoxelSet(1000,0.001f)
	var removedVoxels : ArrayBuffer[VoxelCoord] = new ArrayBuffer[VoxelCoord]

	val guard = new Object

	var fluidData : FluidData = null

	override def initialize() {
		val terrain = env.aux[TerrainData].materialGrid
		fluidData = env.auxData[FluidData]

		//Moved this to world gen
//		computor.settleInitialSimulation(world)


//		for ( source <- fluidData.infiniteFluidSources ) {
//			val sourcePoint = source.location
//			val sourceFluid = source.fluidType
//			fluidData.fluidType(sourcePoint) = env.aux[TerrainData].materialMapping(sourceFluid)
//			fluidData.fluidLevel(sourcePoint) = -1.toShort
//			dirtyVoxels.add(sourcePoint)
//		}

		terrain.onEvent {
			case evt : TaleaModificationsCompletedEvent => {
				evt.talea.foreach {
					case bt : TLoggingTalea[Byte] => {
						val preMod = evt.preModificationCounts(bt)
						removedVoxels.appendAll( bt.loggedModifications.takeWhile( _.revision > preMod ).flatMap (
							lm =>
								if ( ! TerrainByteUtils.isFluidPassable(lm.oldValue) && TerrainByteUtils.isFluidPassable(lm.newValue) ) {
									if ( computor.isPull ) {
										List(VoxelCoord(lm.x + bt.x,lm.y + bt.y,lm.z + bt.z) )
									} else {
										var d = List[VoxelCoord]()
										var q = 0; while ( q < 6 ) {
											d ::= VoxelCoord(	lm.x + bt.x + cardinalsX(q),
												lm.y + bt.y + cardinalsY(q),
												lm.z + bt.z + cardinalsZ(q))
											q += 1}
										d
									}
								} else { Nil }
						) )
					}
					case _ => Noto.error("WAT")
				}
			}
		}

		initPressure()
	}

	def addDirtyVoxels ( voxels : GenTraversableOnce[VoxelCoord] ) {
		guard synchronized {
			voxels.foreach( dirtyVoxels.add )
		}
	}

	def addDirtyVoxels ( voxels : TBareBonesSet[VoxelCoord] ) {
		guard synchronized {
			val tmp = dirtyVoxels
			dirtyVoxels = voxels
			dirtyVoxels.addAll(tmp)
			//			dirtyVoxels ++= voxels
		}
	}

	class WorkerThread extends UpdateThread(0.01666667f.seconds,Killable.GameLevel) {
		def update() {
			doIteration()
		}
	}
	val workerThread = new WorkerThread

	lazy val fluidSync = new CachedBoolean(settingValue[Boolean]("Graphics/Advanced/Sync Fluid"))

	val pressureString = "Gameplay/Fluid/Pressure"
	override def initSettings = List(new BooleanSetting(pressureString,true))
	lazy val pressureEnabled = new CachedBoolean(settingValue[Boolean](pressureString))

	var iterCount = 0
	def doIteration () {
		computor.pressureEnabled = pressureEnabled.resolve()
		iterCount += 1
		if ( iterCount % 60 == 0 ) {
			FluidGameComponent.fps = (1.0f / (FluidGameComponent.avgTime.avg))
			FluidGameComponent.avgTime = new RunningAverage
		}
		val start = System.nanoTime()

		var curVoxels : TBareBonesSet[VoxelCoord] = null
		guard synchronized {
			curVoxels = dirtyVoxels
			removedVoxels.foreach(curVoxels.add)
			dirtyVoxels = new ProbabilisticVoxelSet(1000,0.001f)
		}

		computor.voxelsRemoved(env,removedVoxels)
		removedVoxels.clear()

		if ( fluidSync.resolve() ) {
			fluidData synchronized {
				addDirtyVoxels( computor.stepFluidSimulation(env,curVoxels) )
			}
		} else {
			addDirtyVoxels( computor.stepFluidSimulation(env,curVoxels) )
		}

		FluidGameComponent.avgTime.add(((System.nanoTime() - start) / 1000000000.0).toFloat)
	}

	def update(time: UnitOfTime) {
		workerThread.timePassed(time)
	}

	def initPressure () {

	}
}

object FluidGameComponent {
	var avgTime = new RunningAverage
	var fps = 60.0f
}