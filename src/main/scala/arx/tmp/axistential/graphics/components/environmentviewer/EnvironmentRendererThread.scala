package arx.axistential.graphics.components.environmentviewer

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/13/13
 * Time: 8:55 AM
 * Created by nonvirtualthunk
 */

import java.util.concurrent.locks.LockSupport
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.locks.ReentrantReadWriteLock

import arx.Prelude._
import arx.application.Noto
import arx.axistential.graphics.components.environmentviewer.EnvironmentViewerComponent2.VBOContainer
import arx.core.datastructures.Killable
import arx.core.datastructures.KillableThread
import arx.core.datastructures.SynchronizedQueue
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec3i
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.Talea
import arx.graphics.DynamicVBO
import arx.graphics.TextureBlock

class EnvironmentRendererThread(
	renderQ : SynchronizedQueue[EnvironmentRenderTask],
	delayedRenderQ : SynchronizedQueue[EnvironmentRenderTask],
	window : SlidingWindow2[VBOContainer],
	environmentViewerLayers : Array[TEnvironmentViewerLayer],
	textureBlock : TextureBlock,
	world : World,
	viewLock : ReentrantReadWriteLock,
	inView : Array[Boolean]
) extends KillableThread(Killable.GameLevel)
{
	val limit = ReadVec3i(Talea.dimension,Talea.dimension,Talea.dimension)
	val hdim = Talea.dimension/2
	val radius = sqrtf(powf(hdim,2.0f) * 3.0f)

	def toDelayedRenderTask ( rt : EnvironmentRenderTask ) = if ( rt.layerMask == -1 ) {
		rt
	} else { rt.copy(layerMask = -1) }

	def whileRunningDo() {
//		import EnvironmentRendererThread.delayedSet
//		import EnvironmentRendererThread.delayedLock


		renderQ.dequeueOpt() match {
			case Some(currentTask) => {
				renderTask(currentTask)
			}
			case None => {
//				if ( delayedLock.tryLock() ) {
//					try {
//						val cur = delayedSet
//						delayedSet = Set()
//						val n = cur filterNot renderTask
//						delayedSet ++= n
//					} finally {
//						delayedLock.unlock()
//					}
//				} else {
					LockSupport.parkNanos(10000000)
//				}
			}
		}
	}

	def renderTask ( currentTask : EnvironmentRenderTask ) = {

		val container = currentTask.container
		val pos = VoxelCoord(container.position)

		var rendered = false

		viewLock.readLock().lock()
		try {
			if ( window.contains(pos.x,pos.y,pos.z) ) {
				val index = window.indexOf(pos.x,pos.y,pos.z)
//				if ( inView(index) ) 
					var layerIndex = 0
					while ( layerIndex < container.vbos.length ) {
						if ( (currentTask.layerMask & (1 << layerIndex)) != 0 ) {
							val vbo = container.vbos(layerIndex)
							if ( vbo.state.compareAndSet( DynamicVBO.Clean , DynamicVBO.Updating ) ) {
								val viewLayer = environmentViewerLayers(layerIndex)
								val newRevision = viewLayer.revisionOf(world,pos)

								if ( vbo.lastUpdatedMarker < newRevision ) {
									vbo.numPoints = 0
									vbo.numIndices = 0
									vbo.lastUpdatedMarker = newRevision

									viewLayer.renderer.updateTalea(vbo,textureBlock,world,pos,Vec3i.Zero,limit)

									val endState = if ( ! vbo.wouldSolidifyIfNecessary || vbo.isEmpty ) {
										DynamicVBO.Clean
									} else {
										DynamicVBO.Updated
									}

									if ( ! vbo.state.compareAndSet( DynamicVBO.Updating , endState )) {
										Noto.error("vbo.state was modified while render thread should have had lock")
									}
								} else {
		//							Noto.info("Skipped render, vbo was already in acceptable shape")
									if ( ! vbo.state.compareAndSet( DynamicVBO.Updating , DynamicVBO.Clean ) ){
										Noto.error("\tvbo.state was modified while render thread should have had lock, could not change to clean")
									}
								}
								rendered = true
							} else {
								//This VBO is already somewhere in the modification process, we can't touch it, put it into delayed
//										delayedRenderQ.enqueue(currentTask)
								rendered = false
							}
						}

						layerIndex += 1
					}
//				}
//				else {
//					delayedRenderQ.enqueue(currentTask)
////					delayedSet += toDelayedRenderTask(currentTask)
//					rendered = false
//				}
			} else {
				delayedRenderQ.enqueue(currentTask)
//				delayedSet += toDelayedRenderTask(currentTask)
				rendered = true
				//Disregard this task, the talea has passed out of the view, it will be added back into
				//the queue when next it comes in
			}
		} finally {
			viewLock.readLock().unlock()
		}

		rendered
	}
}


object EnvironmentRendererThread {
	var delayedSet = Set[EnvironmentRenderTask]()
	val delayedLock = new ReentrantLock
}
