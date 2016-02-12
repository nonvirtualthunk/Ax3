package arx.axistential.graphics.components.fluid

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/20/13
 * Time: 3:24 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.anthologicon.graphics.components.subcomponents.TEnvironmentCutoffComponent
import arx.axistential.game.data.world.FluidData
import arx.axistential.graphics.GraphicsSettingsConstants
import arx.axistential.graphics.components.TCoreEnvironmentViewerComponent
import arx.axistential.graphics.components.environmentviewer.TEnvironmentViewerLayer
import arx.axistential.graphics.components.renderers.FluidRenderer
import arx.axistential.graphics.components.weather.TCloudGraphicsComponent
import arx.core.CachedBoolean
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.TaleaGrid
import arx.tmp.game.logic.datastructures.TaleaGrid.TaleaModificationsCompletedEvent
import arx.tmp.game.logic.world.data.LightData
import arx.graphics.GL
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15

class FluidViewerComponent2 extends GraphicsComponent {
	//+=============================+ Dependencies +=============================+
	dependencies ::= classOf[TCoreEnvironmentViewerComponent]
	dependencies ::= classOf[TCloudGraphicsComponent]
	dependencies ::= classOf[TEnvironmentCutoffComponent]

	lazy val coreComp = reify[TCoreEnvironmentViewerComponent]
	lazy val cloudComp = reify[TCloudGraphicsComponent]
	lazy val cutoffComp = reify[TEnvironmentCutoffComponent]
	//+=============================+ State +=============================+
	var containerIndex = -1

	//+=============================+ Settings +=============================+
	val _useDrawElements = new CachedBoolean( settingValue[Boolean](GraphicsSettingsConstants.UseGLDrawElements) , 113 )
	//+=============================+ Ordering +=============================+
	override def drawOrder = GraphicsComponentDrawOrder.AbsolutelyFinal + 1000

	class FluidEnvironmentViewerLayer extends TEnvironmentViewerLayer {
		def revisionOf(env: World, location: VoxelCoord): Long = {
			val fluidData = env.auxData[FluidData]
			val lightData = env.auxData[LightData]
			var sum : Long = fluidData.fluidLevel.getModifiedCountSumIncludingAdjacents(location)
			sum += lightData.lightRevisionAt(location)
			sum
		}
		val renderer = pio[FluidRenderer]
	}
	val fluidLayer = new FluidEnvironmentViewerLayer

	override def initialize() {
		containerIndex = coreComp.addViewLayer(fluidLayer)

		gameEngine.activeWorld.auxData[FluidData].fluidLevel.onEvent {
			case TaleaModificationsCompletedEvent(taleae) => {
				taleae.foreach( t => coreComp.markNeedsUpdate(VoxelCoord(t.position),1 << containerIndex) )
			}
		}
	}

	def draw(graphicsContext: RenderingContext) {
		coreComp.shader.bind()
		coreComp.textureBlock.bind(0)
		cloudComp.cloudTexture.bind(1)

		pov.look()

		GL.glSetState(GL_DEPTH_TEST,enable = true)
		GL.glSetDepthFunc(GL_LEQUAL)
		GL.glSetState(GL_CULL_FACE,enable = true)
		GL.glSetCullFace(GL_BACK)
		GL.glPushState(GL_BLEND,truth = true)

		coreComp.drawViewLayer(fluidLayer,GL15.GL_STREAM_DRAW)

		GL.glPopState(GL_BLEND)
	}

	def setPointOfView(pov: TCamera) {}

	protected def update(f: Float) {}
}
