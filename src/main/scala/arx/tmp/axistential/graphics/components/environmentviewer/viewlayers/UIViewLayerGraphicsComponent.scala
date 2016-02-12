package arx.axistential.graphics.components.environmentviewer.viewlayers

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/16/14
 * Time: 5:10 PM
 */

import arx.anthologicon.graphics.components.subcomponents.TEnvironmentCutoffComponent
import arx.axistential.graphics.GraphicsSettingsConstants
import arx.axistential.graphics.components.TCoreEnvironmentViewerComponent
import arx.core.CachedBoolean
import arx.graphics.GL
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15

class UIViewLayerGraphicsComponent extends GraphicsComponent {
		//+=============================+ Dependencies +=============================+
		dependencies ::= classOf[TCoreEnvironmentViewerComponent]
		dependencies ::= classOf[TEnvironmentCutoffComponent]

		lazy val coreComp = reify[TCoreEnvironmentViewerComponent]
		lazy val cutoffComp = reify[TEnvironmentCutoffComponent]
		//+=============================+ State +=============================+

		//+=============================+ Settings +=============================+
		val _useDrawElements = new CachedBoolean( settingValue[Boolean](GraphicsSettingsConstants.UseGLDrawElements) , 113 )
		//+=============================+ Ordering +=============================+
		override def drawOrder = GraphicsComponentDrawOrder.AbsolutelyFinal + 1000

		override def initialize() {

		}

		def draw(graphicsContext: RenderingContext) {
			coreComp.textureBlock.bind(0)

			pov.look()

			GL.glSetState(GL_DEPTH_TEST,enable = true)
			GL.glSetDepthFunc(GL_LEQUAL)
			GL.glSetState(GL_CULL_FACE,enable = true)
			GL.glSetCullFace(GL_BACK)
			GL.glPushState(GL_BLEND,truth = true)

			coreComp.viewLayers.foreach {
				case layer : TUIViewLayer => {
					layer.shader.bind()
					coreComp.drawViewLayer(layer,GL15.GL_STREAM_DRAW)
				}
				case _ =>
			}

			GL.glPopState(GL_BLEND)
		}

		def setPointOfView(pov: TCamera) {}

		protected def update(f: Float) {}
	}