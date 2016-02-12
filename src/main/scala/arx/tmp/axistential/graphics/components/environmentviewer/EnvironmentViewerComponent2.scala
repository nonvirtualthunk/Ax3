package arx.axistential.graphics.components.environmentviewer

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/12/13
 * Time: 7:47 PM
 * Created by nonvirtualthunk
 */

import java.util.concurrent.locks.ReentrantReadWriteLock

import arx.Prelude._
import arx.anthologicon.graphics.components.subcomponents.TEnvironmentCutawayComponent
import arx.anthologicon.graphics.components.subcomponents.TEnvironmentCutoffComponent
import arx.application.Noto
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.logic.intersection.WorldIntersector
import arx.axistential.graphics.GraphicsSettingsConstants
import arx.axistential.graphics.components._
import arx.axistential.graphics.components.environmentviewer.EnvironmentViewerComponent2.VBOContainer
import arx.axistential.graphics.components.renderers.AnthologiconEnvironmentRenderer
import arx.axistential.graphics.components.renderers.TEnvironmentRenderer
import arx.axistential.graphics.components.weather.TCloudGraphicsComponent
import arx.axistential.graphics.shader.AnthologiconWorldCutawayShader
import arx.axistential.graphics.shader.AnthologiconWorldShader
import arx.core.CachedBoolean
import arx.core.datastructures.SynchronizedQueue
import arx.core.vec.Vec3f
import arx.core.vec.coordinates.MutableVoxelCoord
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.TMajorCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic._
import arx.tmp.game.logic.datastructures.TaleaGrid.TaleaModificationsCompletedEvent
import arx.tmp.game.logic.datastructures._
import arx.tmp.game.logic.world.data.LightData
import arx.graphics._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL11
import org.lwjgl.opengl.GL15
import org.lwjgl.opengl.GL32

import scala.collection.mutable

class EnvironmentViewerComponent2 extends GraphicsComponent with TCoreEnvironmentViewerComponent with TVisibilityComponent {
	//+=============================+ Depenencies +=============================+
	dependencies ::= classOf[SkyboxComponent]
	dependencies ::= classOf[TEnvironmentCutoffComponent]
	dependencies ::= classOf[TEnvironmentCutawayComponent]
	dependencies ::= classOf[TCloudGraphicsComponent]

	lazy val cloudComponent = reify[TCloudGraphicsComponent]
	lazy val cutoffComponent = reify[TEnvironmentCutoffComponent]
	lazy val cutawayComponent = reify[TEnvironmentCutawayComponent]

	//+=============================+ Settings +=============================+
	override def initSettings = new BooleanSetting(GraphicsSettingsConstants.UseGLDrawElements,true) ::
										 new IntRangeSetting(GraphicsSettingsConstants.TerrainRendererThreads,2,1,4) :: Nil
	val _useDrawElements = new CachedBoolean( settingValue[Boolean](GraphicsSettingsConstants.UseGLDrawElements) , 113 )

	//+=============================+ Shaders +=============================+
	lazy val mainShader = new AnthologiconWorldShader(world,pov _)
	lazy val cutoffShader = new AnthologiconWorldShader(world,pov _).
		withZCutoff(cutoffComponent.lastCompletedZCutoff _).withCutoffActive(enable = true)
	lazy val cutawayShader = new AnthologiconWorldCutawayShader(world,pov _).
		withCutawayXYThreshold( pov.cutawayXYThreshold _ ).
		withCutawayZThreshold( pov.cutawayZThreshold _ )

	def shader = if ( pov.isCutawayActive ) {
		cutawayShader
	} else if ( pov.isCutoffActive ) {
		cutoffShader
	} else {
		mainShader
	}
	//+=============================+ Texturing +=====================================+
	textureBlock.internalFormat = GL11.GL_RGBA

	//+=============================+ Main Data Structures +=============================+
	var _viewLayers = Array[TEnvironmentViewerLayer]( new TerrainEnvironmentViewerLayer ); def viewLayers = _viewLayers
	def addViewLayer ( layer : TEnvironmentViewerLayer ) : Int = {
		layer.graphicsEngine = graphicsEngine
		val ret = viewLayers.length; _viewLayers = (viewLayers.toList ::: List(layer)).toArray; ret }

	class VBOType(prof : AttributeProfile) extends AVBO(prof) with Locatable3D


	val vboGrid = new GenericSubContainerStore[VBOContainer](Talea.dimensionPo2,(pos:VoxelCoord) => new VBOContainer(viewLayers,pos))
	var window : SlidingWindow2[VBOContainer] = null
	var inView : Array[Boolean] = null

	val renderQ = new SynchronizedQueue[EnvironmentRenderTask]
	val delayedQ = new SynchronizedQueue[EnvironmentRenderTask]

	val viewLock = new ReentrantReadWriteLock

	//+=============================+ State Tracking +=============================+
	var mostRecentCameraForward = Vec3f(0.0f,0.0f,1.0f)

	//+=============================+ Workers and Rendering +=============================+
	var renderThreads : List[EnvironmentRendererThread] = Nil

	var renderer = new AnthologiconEnvironmentRenderer()
	//+=============================+ Initialization +=============================+
	override def initialize() {
		viewLayers.foreach( _.graphicsEngine = graphicsEngine )

		window = new SlidingWindow2[VBOContainer](vboGrid,voxelEye,(pov.viewDistance + Talea.dimension).round,shouldInitialize = false)
		window.onEnter = vboEntered _
		window.onExit = vboExited _
		window.initialize()

		inView = Array.ofDim[Boolean](window.windowDim*window.windowDim*window.windowDim)

		for ( i <- 0 until settingValue[Int](GraphicsSettingsConstants.TerrainRendererThreads) ) {
			renderThreads ::= new EnvironmentRendererThread(
				renderQ,
				delayedQ,
				window,
				viewLayers,
				textureBlock,
				world,
				viewLock,
				inView
			)
		}
		renderThreads.foreach(_.start())

		world.aux[LightData].onEvent {
			case TaleaModificationsCompletedEvent(taleae) => taleaeChanged(taleae)
		}
		world.aux[TerrainData]._coveringGrid.onEvent {
			case TaleaModificationsCompletedEvent(taleae) => taleaeChanged(taleae)
		}
	}

	def taleaeChanged ( taleae : Traversable[ITalea[_]] ) {
		taleae.foreach( t => renderQ.enqueue( EnvironmentRenderTask(vboGrid(t.x,t.y,t.z) ) ))
	}


	def vboEntered ( v : VBOContainer ) {
		renderQ.enqueue(EnvironmentRenderTask(v))
	}

	def markNeedsUpdate ( v : VoxelCoord , mask : Int ){
		renderQ.enqueue(EnvironmentRenderTask(vboGrid(v.x,v.y,v.z),mask))
	}

	def vboExited ( v : VBOContainer ) {
		var li = 0; while ( li < v.vbos.length ) {
			v.vbos(li).state.set(DynamicVBO.Clean)
			v.vbos(li).unsolidify()
			v.vbos(li).clear()
			li += 1
		}
	}



	var alternator = 0
	val toDraw = new Array[VBOContainer](5000)

	//+=============================+ Drawing +=============================+
	def draw(graphicsContext: RenderingContext) {
		shader.bind()
		textureBlock.bind(0)
		cloudComponent.cloudTexture.bind(1)

		pov.look()

		GL.glSetState(GL_DEPTH_TEST,enable = true)
		GL.glSetDepthFunc(GL_LESS)
		GL.glSetState(GL_CULL_FACE,enable = true)
		GL.glSetCullFace(GL_BACK)
		GL.glPushState(GL_BLEND,truth = false)
		GL.glSetState(GL32.GL_DEPTH_CLAMP,enable = true)

		drawViewLayer( viewLayers.head , GL15.GL_STATIC_DRAW )

		GL.glPopState(GL_BLEND)

		if ( GL.debugMode ) { GL.checkError() }
		alternator += 1
	}

	def drawViewLayer ( layer : TEnvironmentViewerLayer, usage : Int ) {
		val layerIndex = viewLayers.indexOf(layer)
		val useDrawElements = _useDrawElements.resolve()

		if ( layerIndex == 0 && (alternator & 3) == 0 ) {
			val effectiveZCutoff = ObjectCoord.toVoxelCoordZ(cutoffComponent.lastCompletedZCutoff)

			val windowIndices = window.indicesApproximatelyOrderedByDistance

			var counter = 0
			viewLock.readLock().lock()
			try{
				var i = 0
				i = 0; while ( i < windowIndices.length && windowIndices(i) != -1 ) {
					val index = windowIndices(i)
					val container = window(index)

					val vbo = container.vbos(0)
					if ( inView(index) && container.position.z <= effectiveZCutoff ) {
						toDraw(counter) = container
						counter += 1
						individualDraw(layer,container,vbo,useDrawElements,usage)
					}
					i += 1}
				toDraw(counter) = null
				VAO.unbind()
				//			println("Outside : " + count + " , inside : " + inside)
			} finally {
				viewLock.readLock().unlock()
			}
		} else {
			var i = 0; while ( i < toDraw.length ) {
				val container = toDraw(i)
				if ( container == null ) { i = toDraw.length }
				else {
					individualDraw(layer,container,container.vbos(layerIndex),useDrawElements,usage)
				}

				i += 1
			}
			VAO.unbind()
		}
	}

	@inline
	def individualDraw (layer : TEnvironmentViewerLayer, container : VBOContainer, vbo : AVBO, useDrawElements : Boolean, usage : Int) {
		if ( vbo.solidifyIfNecessary(usage) ) {
			if ( vbo.state.compareAndSet(DynamicVBO.Clean,DynamicVBO.Updating) ) {
				vbo.clear()
				if ( layer.revisionOf(world,container.position) > vbo.lastSolidifiedMarker ) {
					markNeedsUpdate(container.position,-1)
				}
				vbo.state.compareAndSet(DynamicVBO.Updating,DynamicVBO.Clean)
			}
		}
		if ( vbo.isSolidified ) {
			if ( useDrawElements ) {
				vbo.drawElements(GL_TRIANGLES,0,-1,skipPostDraw = true)

			} else {
				vbo.draw(GL_TRIANGLES,0,-1)
			}
		}
	}

	def setPointOfView(pov: TCamera) {}

	val delayedSet = new mutable.HashSet[VBOContainer]
	protected def update(f: Float) {
//		renderTaskThread.timePassed((f * 0.016666667f).seconds)
		updateEye()

		delayedSet.clear()
//		val extraTmpQ
		var break = false
		delayedQ synchronized {
			while ( ! break ) {
				delayedQ.dequeueOpt() match {
					case Some(d) => {
						if ( ! delayedSet.contains(d.container) ) {
							if ( d.layerMask != -1 ) {
								renderQ.enqueue(d.copy(layerMask = -1))
							} else {
								renderQ.enqueue(d)
							}
							delayedSet.add(d.container)
						}
					}
					case _ => break = true
				}
			}
		}
//		delayedQ.synchronized {
//			while ( delayedQ.nonEmpty ) {
//				val d = delayedQ.dequeue()
//				if ( ! delayedSet.contains(d.container) ) {
//					if ( d.layerMask != -1 ) {
//						renderQ.enqueue(d.copy(layerMask = -1))
//					} else {
//						renderQ.enqueue(d)
//					}
//					delayedSet.add(d.container)
//				}
//			}
//		}
	}

	protected def updateEye () {
		val forward = pov.forward
		val theta = math.acos( mostRecentCameraForward dot forward ).toFloat

		val terrain = world.aux[TerrainData]

		if ( absf(theta) > 0.001f ) {
			val hdim = (Talea.dimension * 0.75).toInt
			val radius = sqrtf(powf(hdim,2.0f) * 3.0f)
			val angle = math.toRadians(pov.fovy * 0.8f).toFloat
			val cosAngle = cosf(angle)
			val tanAngle = tanf(angle)

//			var counter = 0
//			val start = System.nanoTime
			mostRecentCameraForward = forward

			viewLock.readLock().lock()
			try{
				val windowIndices = window.indicesApproximatelyOrderedByDistance
				val relativePositions = window.relativePositions
				var i = 0
				while ( i < windowIndices.length && windowIndices(i) != -1 ) {
					val index = windowIndices(i)
					val intersect = Intersection.coneSphereInclusionTest(
						forward.x,forward.y,forward.z,
						cosAngle,tanAngle,
						relativePositions(0)(index),relativePositions(1)(index),relativePositions(2)(index),
						radius)

					val nowInView = intersect != 0
					inView(index) = nowInView

					i += 1
				}
			} finally {
				viewLock.readLock().unlock()
			}
//			println("Intersection test time taken : " + ((System.nanoTime - start) / 1.0e9) + "s")
		}

		val newEye = voxelEye
		if ( newEye != window.center ) {
			val lock = if ( window.wouldMovingCenterRequireSlide(newEye) ) { viewLock.writeLock } else { viewLock.readLock }
			lock.lock()
			try {
				window.moveCenterTo(newEye)
			} finally {
				lock.unlock()
			}
		}
	}


	def inVisibleRegion(wc: TMajorCoord): Boolean = wc.distanceTo(voxelEye).inVoxels < pov.viewDistance + (Talea.dimension*2)


	def foreachVisibleTalea[U](f: (VoxelCoord) => U) {
		val veye = (voxelEye >> Talea.dimensionPo2) << Talea.dimensionPo2

		val vx = veye.x;val vy = veye.y;val vz = veye.z
		val coord = new MutableVoxelCoord
		val relativePositions = window.relativePositions
		val indices = window.indicesApproximatelyOrderedByDistance
		var ii = 0; while ( ii < indices.length && indices(ii) != -1 ) {
			val index = indices(ii)
			if ( inView(index) ) {
				coord.x = vx + relativePositions(0)(index)
				coord.y = vy + relativePositions(1)(index)
				coord.z = vz + relativePositions(2)(index)
				f(coord)
			}
		ii += 1}
	}

	//get this out of here? why do this? who knows?
	def renderers: List[TEnvironmentRenderer] = List(renderer)

	//+=============================+ Convenience +=============================+
	def voxelEye = ObjectCoord(pov.eye).toVoxelCoord


	//+=============================+ Intersection +=============================+
	override def intersectWorld(start_oc: ObjectCoord, baseEnd_oc: ObjectCoord,
										 auxGrid : TInfiniteVoxelView[Byte] = DummyInfiniteByteVoxelStore,
										 filterFunction : (WorldIntersectionResult) => Boolean = passthroughIntersectionFilter) =
	{
		if ( ! graphicsEngine.pov.zCutoffActive ) {
			if ( ! graphicsEngine.pov.isCutawayActive ) {
				WorldIntersector.intersect(world,start_oc,baseEnd_oc,auxGrid,filterFunction)
			} else {
				val eye = ObjectCoord(pov.eye)
				val maxXYDistSq = pov.cutawayXYThreshold * pov.cutawayXYThreshold
				val maxZDist = pov.cutawayZThreshold
				WorldIntersector.intersect(world,start_oc,baseEnd_oc,auxGrid,(wir) => {
					if ( ! filterFunction(wir) ) { false }
					else {
						val dx = eye.x - wir.point.x
						val dy = eye.y - wir.point.y
						val dz = eye.z - wir.point.z
						dx * dx + dy * dy >= maxXYDistSq || absf(dz) >= maxZDist
					}
				})
			}
		} else {
			val f = if ( baseEnd_oc.z != start_oc.z ) { ((graphicsEngine.pov.zCutoff + 0.49f) - start_oc.z) / (baseEnd_oc.z - start_oc.z) } else { -1.0f }
			if ( f >= 0.0f ) {
				val newStart = ObjectCoord(start_oc + (baseEnd_oc - start_oc) * f)
				val vox = world.auxData[TerrainData].materialByteAt(newStart.toVoxelCoord)
				val aux = auxGrid(newStart.toVoxelCoord)
				if ( vox > 0 ) {
					Noto.finest(GraphicsLogging,"Hit break out intersection, zcutoff: " + graphicsEngine.pov.zCutoff + " collision: " + newStart.toStringer)
					Some(WorldIntersectionResult(newStart.toVoxelCoord,newStart,Cardinals.Top).withVoxelValue(vox).withAuxGridValue(aux))
				} else {
					val delta = (baseEnd_oc - start_oc).normalize
					Noto.finest(GraphicsLogging,"Running normal intersection")

					val cutoffFunc = (wir:WorldIntersectionResult) => {
						filterFunction(wir) &&
							(wir.voxel.z <= ObjectCoord.toVoxelCoordZ(graphicsEngine.pov.zCutoff) && wir.side != Cardinals.Bottom)
					}
					WorldIntersector.intersect(world,ObjectCoord(newStart + delta * 1.5f),baseEnd_oc,auxGrid,cutoffFunc)
				}
			} else { None }
		}
	}
}

object EnvironmentViewerComponent2 {
	class VBOContainer(layers:Array[TEnvironmentViewerLayer],pos : VoxelCoord){
		val position : VoxelCoord = pos
		val vbos = Array.ofDim[AVBO](layers.length)
		var i = 0; while ( i < layers.length ) { vbos(i) = new AVBO(layers(i).renderer.attributeProfile) ; i += 1 }

		val _hashCode = pos.hashCode
		override def hashCode = _hashCode
	}
}
