package arx.anthologicon.graphics.components.subcomponents

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/14/12
 * Time: 9:38 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.graphics.components.TCoreEnvironmentViewerComponent
import arx.axistential.graphics.components.renderers.AnthologiconCutoffEnvironmentRenderer
import arx.axistential.graphics.components.renderers.TCutoffEnvironmentRenderer
import arx.axistential.graphics.components.weather.TCloudGraphicsComponent
import arx.axistential.graphics.shader.AnthologiconWorldShader
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.Vec2i
import arx.core.vec.Vec3i
import arx.tmp.game.logic.datastructures.Talea
import arx.graphics._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL11
import org.lwjgl.opengl.GL15

trait TEnvironmentCutoffComponent extends GraphicsComponent {
	def lastCompletedZCutoff : Float
}

class EnvironmentCutoffComponent extends GraphicsComponent with TEnvironmentCutoffComponent {
	dependencies = List(classOf[TCoreEnvironmentViewerComponent],classOf[TVisibilityComponent],classOf[TCloudGraphicsComponent])
	lazy val cloudComp = reify[TCloudGraphicsComponent]
	lazy val coreComp = reify[TCoreEnvironmentViewerComponent]
	lazy val visiComp = reify[TVisibilityComponent]
	var lastRenderedZCutoff = 10000.0f
	var lastSolidifiedZCutoff = 10000.0f
	def lastCompletedZCutoff = lastRenderedZCutoff
	var cutoffRenderer : TCutoffEnvironmentRenderer = new AnthologiconCutoffEnvironmentRenderer()
	val cutoffVBO = new AVBO(DefaultAttributeProfile)
	lazy val shader = new AnthologiconWorldShader(world,pov _).withZCutoff(1000000.0f)

	def cutoffModeActive = graphicsEngine.pov.zCutoffActive && graphicsEngine.pov.zCutoff < 10000.0f
	def cutoffDistance = graphicsEngine.pov.zCutoffDistance



	override def drawOrder = coreComp.drawOrder + 1

	def initializeOffsets(vd:Float) : List[Vec3i] = {
		val iradius = vd.toInt
		val shiftedStart = (Vec2i(iradius,iradius) >> Talea.dimensionPo2) * -1
		val shiftedEnd = Vec2i(iradius,iradius) >> Talea.dimensionPo2

		var ret : List[Vec3i] = Nil
		val center = Vec2i(0,0)
		val error = (Vec2i(Talea.dimension/2,Talea.dimension/2)).length
		for ( sx <- shiftedStart.x to shiftedEnd.x ; sy <- shiftedStart.y to shiftedEnd.y ; x = sx * Talea.dimension; y = sy * Talea.dimension) {
			val dist = distance(x,y,center)
			if ( dist - error < vd ) {
				ret ::= Vec3i(x,y,0)
			}
		}
		ret
	}
	override def initialize() {
		super.initialize()

//		coreComp.renderTasks ::= (() => {
//			if ( cutoffVBO.state.get == DynamicVBO.Dirty ) {
//				renderCutoff()
//				true
//			} else { false }
//		})
	}

	def draw(graphicsContext: RenderingContext) {
		val cutoff = cutoffModeActive
		if ( cutoff ) {
			Noto.warn("Cutoff component needs to be updated to use triangles and incies")
			val textureBlock = coreComp.textureBlock

			shader.bind()
			textureBlock.bind(0)
			cloudComp.cloudTexture.bind(1)

//			glLoadIdentity()
			graphicsContext.pov.look()

			GL.glSetState(GL_DEPTH_TEST,enable = true)
			GL.glSetDepthFunc(GL_LEQUAL)
			GL.glSetState(GL11.GL_CULL_FACE,enable = false)
			if ( cutoffVBO.solidifyIfNecessary(GL15.GL_STREAM_DRAW) ) {
				lastSolidifiedZCutoff = lastRenderedZCutoff
			}
			if ( cutoffVBO.isSolidified ) {
//				cutoffVBO.drawElements(GL_QUADS)
				cutoffVBO.draw(GL_QUADS)
			}
			GL.glSetState(GL11.GL_CULL_FACE,enable = true)
		} else {
			lastSolidifiedZCutoff = 10000.0f
			lastRenderedZCutoff = 10000.0f
		}
	}
	def setPointOfView(pov: TCamera) {}
	protected def update(f: Float) {
		if ( cutoffModeActive ) {
			if ( cutoffVBO.state.compareAndSet(DynamicVBO.Clean,DynamicVBO.Dirty) ) {
				renderCutoff()
			}
		} else {
			lastSolidifiedZCutoff = 10000.0f
			lastRenderedZCutoff = 10000.0f
		}
	}

	def voxelEye = ObjectCoord(graphicsEngine.pov.eye).toVoxelCoord

	def renderCutoff () {
		Analytics.timeDataPoint("Cutoff VBO Update") {
			val textureBlock = coreComp.textureBlock
			val env = world
			val weye = voxelEye - Vec3i(0,0,cutoffDistance)
			val wcutoff = weye.z
			val cutLevel = graphicsEngine.pov.zCutoff
			if ( cutoffVBO.state.compareAndSet(DynamicVBO.Dirty,DynamicVBO.Updating) ) {
				val cvbo = cutoffVBO
				cvbo.writingActive = true
				cvbo.numPoints = 0
				cvbo.numIndices = 0

				coreComp.foreachVisibleTalea( loc => if ( wcutoff - loc.z >= 0 && wcutoff - loc.z < Talea.dimension ) { cutoffRenderer.updateTaleaCutoff(cvbo,textureBlock,env,loc,wcutoff) } )

				cvbo.lastUpdatedMarker += 1
				cvbo.writingActive = false
				cutoffVBO.state.set(DynamicVBO.Updated)
				lastRenderedZCutoff = cutLevel
			}
		}
	}
}
