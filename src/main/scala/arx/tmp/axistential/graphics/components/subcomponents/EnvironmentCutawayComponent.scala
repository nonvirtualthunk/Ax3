package arx.anthologicon.graphics.components.subcomponents

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/14/12
 * Time: 10:56 AM
 * Created by nonvirtualthunk
 */

import java.util.concurrent.locks.ReentrantLock

import arx.Prelude._
import arx.application.Noto
import arx.axistential.graphics.GraphicsSettingsConstants
import arx.axistential.graphics.components.TCoreEnvironmentViewerComponent
import arx.axistential.graphics.components.renderers.AnthologiconEnvironmentRenderer
import arx.axistential.graphics.components.renderers.THighDetailEnvironmentRenderer
import arx.axistential.graphics.components.renderers.VoxelRenderingSettings
import arx.axistential.graphics.shader.AnthologiconWorldCutawayShader
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.functions
import arx.tmp.game.logic.datastructures.HashVoxelStore
import arx.graphics._
import arx.graphics.shader.Shader
import org.lwjgl.opengl.GL11
import org.lwjgl.opengl.GL15

trait TEnvironmentCutawayComponent extends GraphicsComponent {
	def setShaderVariables ( shader : Shader )
}
class EnvironmentCutawayComponent extends GraphicsComponent with TEnvironmentCutawayComponent {

	dependencies = List(classOf[TCoreEnvironmentViewerComponent])
	lazy val coreComp = reify[TCoreEnvironmentViewerComponent]
//	val cutawayVBOs = Array(new AVBO(DefaultAttributeProfile),new AVBO(DefaultAttributeProfile),new AVBO(DefaultAttributeProfile),new AVBO(DefaultAttributeProfile))
	val cutawayVBOs = Array(new AVBO(DefaultAttributeProfile))
	var renderer: THighDetailEnvironmentRenderer = null
	lazy val shader = {
		val s = new AnthologiconWorldCutawayShader(world,pov _)//ResourceManager.shader("shaders/AnthologiconWorldShaderCutaway")
		s.cutawayXYThreshold.set(0.00001f)
		s.cutawayZThreshold.set(0.00001f)
		s
	}
	lazy val indices = bindFloat( pov.cutawayXYThreshold * -pov.cutawayZThreshold , buildIndices )
	lazy val shellIndices = bindFloat( pov.cutawayXYThreshold * -pov.cutawayZThreshold , buildShellIndices )
	var shellTalea = new HashVoxelStore[Byte]()

	def cutawayActive = (pov.cutawayXYThreshold > 0.0f || pov.cutawayZThreshold > 0.0f) && ! pov.zCutoffActive


	override def initSettings = List(new RangeSetting(GraphicsSettingsConstants.XYCutawayDistance,15.0f,2.0f,30.0f) , new RangeSetting(GraphicsSettingsConstants.ZCutawayDistance,10.0f,2.0f,30.0f) ,
		new RangeSetting(GraphicsSettingsConstants.CutawayOpacity,0.1f,0.0f,1.0f))

	def buildIndices ( unused : Float ) : IndexedSeq[Vec3i] = {
		val xyR = pov.cutawayXYThreshold.toInt; val zR = pov.cutawayZThreshold.toInt
		var ret : List[Vec3i] = Nil
		for ( x <- -xyR to xyR ; y <- -xyR to xyR ; z <- -zR to zR ) {
			val xyDist = lengthi(x,y)
			val zDist = absf(z)
			//if ( absf(xyDist - xyR) < 1.0f || absf(zDist - zR) < 1.0f ) {
			if ( absf(xyDist) < xyR && absf(zDist) < zR ) {
				ret ::= Vec3i(x,y,z)
			}
		}

		ret = ret.sortBy( v=>functions.length(v) ).reverse
		ret.toIndexedSeq
	}
	def buildShellIndices ( unused : Float ) : IndexedSeq[IndexedSeq[Vec3i]] = {
		val xyR = pov.cutawayXYThreshold.toInt + 3; val zR = pov.cutawayZThreshold.toInt + 1
		var ret : List[Vec3i] = Nil
		for ( x <- -xyR to xyR ; y <- -xyR to xyR ; z <- -zR to zR ) {
			val xyDist = lengthi(x,y)
			val zDist = absf(z)
			if ( xyDist < xyR + 2.0f && zDist < zR + 1.0f ) {
				if ( xyDist > xyR - 2.0f || zDist > zR - 2.0f ) {
					ret ::= Vec3i(x,y,z)
				}
			}
//			if ( (absf(xyDist) >= xyR && absf(xyDist) < xyR + 2.0f) || (absf(zDist) >= zR && absf(zDist) < zR + 2.0f) ) {
//				ret ::= Vec3i(x,y,z)
//			}
		}

		shellTalea = new HashVoxelStore[Byte]()
		for ( r <- ret ) { shellTalea(r) = 1.toByte }

		ret = ret.sortBy( v=>functions.length(v) ).reverse
		val (start,end) = ret.toIndexedSeq.splitAt(ret.size/2)
		val (one,two) = start.splitAt(start.size/2)
		val (three,four) = end.splitAt(end.size/2)
//		Array(one,two,three,four)
		Array(ret.toIndexedSeq)
	}


	def setShaderVariables(s: Shader) {
		if ( cutawayActive ) {
			s.setUniform("cutawayXYThreshold",pov.cutawayXYThreshold,tolerateAbsence = true)
			s.setUniform("cutawayZThreshold",pov.cutawayZThreshold,tolerateAbsence = true)
		} else {

		}
	}

	override def drawOrder = GraphicsComponentDrawOrder.Last

	override def initialize() {
		super.initialize()
		renderer = coreComp.renderers.firstOfType[THighDetailEnvironmentRenderer].getOrElse(new AnthologiconEnvironmentRenderer)
		coreComp.renderTasks ::= updateCutaway _
	}

	def draw(graphicsContext: RenderingContext) {
		if ( cutawayActive ) {
			Noto.warn("Cutaway component needs to be updated to use triangles and incies")
			shader.bind()

//			GL11.glLoadIdentity()
			pov.look()
			GL.glSetState(GL11.GL_DEPTH_TEST,true)
			coreComp.textureBlock.bind()

//			GL.glPushState(GL11.GL_CULL_FACE,false)


			cutawayVBOs.foreach( _.solidifyIfNecessary(GL15.GL_STREAM_DRAW) )
			cutawayVBOs.foreach( _.draw(GL11.GL_QUADS,0,-1) )

//			GL.glPopState(GL11.GL_CULL_FACE)
		}
	}
	def setPointOfView(pov: TCamera) {}

	var lock = new ReentrantLock()
	protected def update(f: Float) {
		if ( cutawayActive ) {
			cutawayVBOs.head.state.compareAndSet(DynamicVBO.Clean,DynamicVBO.Dirty)
		}
	}
	def updateCutaway () : Boolean = {
		var ret = false;
		if ( lock.tryLock() ) {
			try {
				if ( cutawayVBOs.head.state.compareAndSet(DynamicVBO.Dirty,DynamicVBO.Updating) ) {
					Analytics.timeDataPoint("cutaway rendering") {
//						val faces = for ( i <- 0 until 6 ) yield {
//							if ( functions.dot(Cardinals.cubeFaceNormals(i) * -1.0f,graphicsEngine.pov.forward) > 0.1f ) { false } else { true }
//						}

						for ( (shellIndicesBlock,cutawayVBO) <- (shellIndices zip cutawayVBOs) ) {
							cutawayVBO.numPoints = 0
							cutawayVBO.numIndices = 0

							val outerSettings = VoxelRenderingSettings(exposedFacesOnly = false,opacity = 1.0f,useIndices=false)
//							Noto.info("Shell indices : " + shellIndices.size)
							renderer.updateVoxels(cutawayVBO,coreComp.textureBlock,world,shellIndicesBlock,ObjectCoord(pov.eye).toVoxelCoord,outerSettings)
							val cutawayOpacity = settingValue[Float](GraphicsSettingsConstants.CutawayOpacity)

			//				if ( cutawayOpacity > 0.01f ) {
			//					val innerSettings = VoxelRenderingSettings(exposedFacesOnly = true,opacity = settingValue[Float](GraphicsSettingsConstants.CutawayOpacity))
			//					renderer.updateVoxels(cutawayVBO,coreComp.textureBlock,gameEngine.activeEnvironment,indices,ObjectCoord(pov.eye).toVoxelCoord,innerSettings)
			//				}

							cutawayVBO.lastUpdatedMarker += 1
							cutawayVBO.state.set(DynamicVBO.Updated)
						}
					}
					ret = true
				}
			} finally { lock.unlock(); }
		}
		ret
	}
}
