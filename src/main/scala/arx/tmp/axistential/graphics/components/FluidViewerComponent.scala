package arx.axistential.graphics.components

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/4/12
 * Time: 9:33 AM
 * Created by nonvirtualthunk
 */

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.LockSupport

import arx.Prelude._
import arx.anthologicon.graphics.components.subcomponents.EnvironmentCutoffComponent
import arx.application.Noto
import arx.axistential.game.data.world.FluidData
import arx.axistential.graphics.GraphicsSettingsConstants
import arx.axistential.graphics.components.renderers.FluidRenderer
import arx.axistential.graphics.components.weather.TCloudGraphicsComponent
import arx.core.CachedBoolean
import arx.core.datastructures.Killable
import arx.core.datastructures.KillableThread
import arx.core.datastructures.SynchronizedQueue
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.TaleaGrid.TaleaModificationsCompletedEvent
import arx.tmp.game.logic.datastructures.ITalea
import arx.tmp.game.logic.datastructures.Talea
import arx.tmp.game.logic.datastructures.TaleaGrid
import arx.tmp.game.logic.world.data.LightData
import arx.graphics.DynamicVBO._
import arx.graphics._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15

class FluidViewerComponent extends GraphicsComponent {
	type VBOType = AVBO with Locatable3D with Viewable

	dependencies ::= classOf[TCoreEnvironmentViewerComponent]
	dependencies ::= classOf[EnvironmentCutoffComponent]
	dependencies ::= classOf[TCloudGraphicsComponent]
	lazy val cloudComponent = reify[TCloudGraphicsComponent]
	lazy val coreComponent = reify[TCoreEnvironmentViewerComponent]
	lazy val cutoffComponent = reify[EnvironmentCutoffComponent]
	def textureBlock = coreComponent.textureBlock

	lazy val viewStructure = new ViewStructure[VBOType](graphicsEngine,createVBO _,onVBOEnter _,onVBOExit _)
	lazy val renderer = provideInstanceOf[FluidRenderer]
	lazy val liquidData = activeWorld.auxData[FluidData]
	def shader = coreComponent.shader
	val useDrawElements = new CachedBoolean( settingValue[Boolean](GraphicsSettingsConstants.UseGLDrawElements) , 114 )


	val fluidSyncString = "Graphics/Advanced/Fluid Sync"
	override def initSettings = List(new BooleanSetting(fluidSyncString,false))

	lazy val syncFluid = new CachedBoolean(settingValue[Boolean](fluidSyncString))

	override def initialize() {
		liquidData.fluidLevel.onEvent {
			case TaleaModificationsCompletedEvent(taleae) => {
				triggerUpdatesOnTaleae(taleae)
			}
		}
		activeWorld.auxData[LightData].onEvent {
			case TaleaModificationsCompletedEvent(taleae) => {
				triggerUpdatesOnTaleae(taleae.filter(v => liquidData.definedAt(v.position)))
			}
		}
	}

	def triggerUpdatesOnTaleae ( taleae : Iterable[ITalea[_]] ) {
		if ( ! syncFluid.resolve() ) { viewStructure.lockRead() }
		try {
			taleae.foreach { talea => {
				if ( viewStructure.vboWindow.contains(talea.x,talea.y,talea.z) ) {
					val vbo = viewStructure.vboWindow.intern(talea.x,talea.y,talea.z)
					if ( vbo.state.compareAndSet(Clean,Dirty) ) {
						renderQ += VoxelCoord(talea.x,talea.y,talea.z)
					}
				}
			}}
		} finally {
			if ( ! syncFluid.resolve() ) { viewStructure.unlockRead() }
		}
	}

	override def drawOrder = GraphicsComponentDrawOrder.AbsolutelyFinal + 1000

	def lightData = activeWorld.auxData[LightData]

	val renderQ = new SynchronizedQueue[VoxelCoord]
	var moveTo : AtomicReference[TCamera] = new AtomicReference(null)

	def createVBO ( v : Vec3i ) : VBOType = {
		(new AVBO(StandardAttributeProfile) with Locatable3D with Viewable).withLocation(v)
	}
	def onVBOEnter ( vbo : VBOType ) {
		if ( vbo.state.compareAndSet(Clean,Dirty) ) {
			renderQ.enqueue(VoxelCoord(vbo.location))
		}
	}
	def onVBOExit ( vbo : VBOType ) {
		vbo.state.compareAndSet(Updated,Clean)
		if ( vbo.state.get == Clean ) {
			vbo.unsolidify()
			vbo.clear()
		}
	}

	class WorkerThread(i:Int) extends KillableThread(Killable.GameLevel) {
		def whileRunningDo() {
			try {
				if ( i == 0 ) { //ensure that only one thread does this
					val tmp = moveTo.getAndSet(null)
					if ( tmp != null ) {
						viewStructure.setPointOfView(tmp)
					}
				}
				if ( renderQ.nonEmpty ) {
					renderQ.dequeueOpt() match {
						case Some(location) =>
							try{
								viewStructure.lockRead()
								renderLocation(location)
							} finally {
								viewStructure.unlockRead()
							}
						case None =>
					}
				} else { LockSupport.parkNanos((0.015 * 1000000000).toLong) }
			} catch {
				case e : Exception =>
					Noto.warn("Exception encountered during liquid render loop :")
					e.printStackTrace()
			}
		}
		start()
	}
	val workerThreads = (0 until 2).map( new WorkerThread(_) )


	override def exit() {
		workerThreads.foreach(_.ended = true)
	}

	def revisionAt ( location : VoxelCoord ) = {
		var sum : Long = liquidData.fluidLevel.getModifiedCountSumIncludingAdjacents(location)
		sum += lightData.lightRevisionAt(location)
		sum
	}
	def renderLocation ( location : VoxelCoord ) {
		val window : SlidingWindow[VBOType] = viewStructure.vboWindow
		if ( window.contains(location) ) {
			val vbo = window(location)
			if ( vbo.state.compareAndSet(Dirty,Updating) ) {
				val revision = revisionAt(location)
				if ( (vbo.isSolidified && vbo.lastSolidifiedMarker < revision) ||
						(! vbo.isSolidified && vbo.lastUpdatedMarker < revision) )
				{
					vbo.lastUpdatedMarker = revision
					vbo.numPoints = 0
					vbo.numIndices = 0

//					Noto.info("Actually updating a liquid talea")
					if ( syncFluid.resolve() ) {
						world.auxData[FluidData] synchronized {
							renderer.updateTalea(vbo,textureBlock,world,location,Vec3i.Zero,Vec3i(Talea.dimension,Talea.dimension,Talea.dimension))
						}
					} else {
						renderer.updateTalea(vbo,textureBlock,world,location,Vec3i.Zero,Vec3i(Talea.dimension,Talea.dimension,Talea.dimension))
					}

					if ( ! vbo.wouldSolidifyIfNecessary || ! viewStructure.vboWindow.contains(location) ) {
						Noto.warn("Liquid VBO was stale by the time it was rendered, throwing it out")
						if ( vbo.state.get == Updating ) {
							vbo.clear()
							if ( ! vbo.state.compareAndSet(Updating,Clean) ) { Noto.error("Stale vbo, invalid state") }
						}
					} else {
						if ( vbo.isEmpty ) {
							vbo.state.compareAndSet(Updating,Clean)
						} else {
							vbo.state.compareAndSet(Updating,Updated)
						}
					}
				} else {
//					Noto.warn("No cause to update liquid vbo in render Q : " + vbo.location)
					vbo.state.compareAndSet(Updating,Clean)
				}
			} else {
				Noto.warn("VBO was not dirty come rendering time")
			}
		} else {
			val vbo = viewStructure.vboGrid.getSubContainer(location.x,location.y,location.z)
			if ( vbo != null ) {
				if ( vbo.state.compareAndSet(Dirty,Updating) ) {
					vbo.unsolidify()
					vbo.clear()
					vbo.state.compareAndSet(Updating,Clean)
				}
			}
		}
	}

	def draw(graphicsContext: RenderingContext) {
		val pov = graphicsContext.pov

		shader.bind()
		textureBlock.bind()
		cloudComponent.cloudTexture.bind(1)

		GL.glSetState(GL_DEPTH_TEST,enable = true)
		GL.glSetDepthFunc(GL_LEQUAL)
		GL.glSetState(GL_CULL_FACE,enable = true)
		GL.glSetCullFace(GL_BACK)
		GL.glPushState(GL_BLEND,truth = true)

		pov.look()

		val allVBOs = viewStructure.vboWindow.activeSubContainers

		val windowIndices : Array[Int] = viewStructure.windowIndices
		val effectiveZCutoff = ObjectCoord.toVoxelCoordZ(cutoffComponent.lastCompletedZCutoff)

		var lastDrawnVBO : AVBO = null
		var i = 0;while ( i < windowIndices.length ) {
			val vbo = allVBOs( windowIndices(i) )
//			if ( vbo.nonEmpty ) {
				val loc = vbo.location
				if ( 	vbo.viewAngle == null ||
						vbo.viewAngle.min < viewStructure.maximumViewAngle
				){
					if ( loc.z <= effectiveZCutoff ) {
						if ( vbo.solidifyIfNecessary( GL15.GL_STREAM_DRAW ) ) {
							//If we solidified, check to see if we are not currently up to date. If we are not, add it back into the drawing process
							val currentRev = revisionAt( VoxelCoord(vbo.location) )
							if ( currentRev > vbo.lastSolidifiedMarker ) {
								if ( vbo.state.compareAndSet( DynamicVBO.Clean, DynamicVBO.Dirty ) ) {
									renderQ += VoxelCoord(vbo.location)
								} else { vbo.clear() }
							} else { vbo.clear() }
						}

						if ( vbo.isSolidified ){
							if ( useDrawElements.resolve() ) {
//								Noto.info("Drawing elements for liquid")
								vbo.drawElements(GL_TRIANGLES,0,-1,skipPostDraw = true)
								lastDrawnVBO = vbo
							} else {
								vbo.draw(GL_TRIANGLES,0,-1)
							}
						}
					}
				}
//			}
		i += 1}
		if ( lastDrawnVBO != null ) { lastDrawnVBO.unbind() }

		GL.glPopState(GL_BLEND)
	}
	def setPointOfView(pov: TCamera) {
		moveTo.set(pov)
	}
	protected def update(f: Float) {}
}