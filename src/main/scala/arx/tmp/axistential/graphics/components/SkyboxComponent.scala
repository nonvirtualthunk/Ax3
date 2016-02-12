package arx.axistential.graphics.components

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/18/12
 * Time: 10:12 AM
 * Created by nonvirtualthunk
 */

import arx.application.Noto
import arx.core.vec.Vec3f
import arx.core.vec.functions
import arx.graphics.AVBO
import arx.graphics.GL
import arx.resource.ResourceManager
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL11
import org.lwjgl.opengl.GL15

class SkyboxComponent extends GraphicsComponent {
	name = "Skybox Component"
	var vbo = new AVBO(SimpleAttributeProfile)
	var radius = -1.0f

	val povCopy = new AnthologiconCamera()

	lazy val fogComponent = new FogUniformProvider(gameEngine.activeWorld,povCopy)

	isActive = () => settingValue[Boolean]("Graphics/Advanced/Draw Skybox")
	override def initSettings = List(new BooleanSetting("Graphics/Advanced/Draw Skybox",true))

	override def drawOrder = GraphicsComponentDrawOrder.Last

	def draw(graphicsContext: RenderingContext) {
		GL.glPushState(GL_BLEND,truth = false)
//		glDepthMask(false)

		val pov = graphicsContext.pov

		val shader = ResourceManager.shader("shaders/SkyboxShader")
		shader.bind()
		fogComponent.setFogVariables(shader)

		GL.glSetState(GL11.GL_DEPTH_TEST,enable = true)
		GL.glSetState(GL11.GL_CULL_FACE,enable = true)
		GL.glSetCullFace(GL11.GL_BACK)

		povCopy.eye = Vec3f(0.0f,0.0f,0.0f)
		povCopy._forward = pov.forward
		povCopy._up = pov.up
		povCopy.viewDistance = pov.viewDistance

		povCopy.look()

		if ( vbo.solidifyIfNecessary(GL15.GL_STATIC_DRAW) ) {
			Noto.info("Skybox solidified with radius : " + radius)
		}
		if ( vbo.isSolidified ) {
			vbo.drawElements(GL11.GL_TRIANGLES)
		}

//		glDepthMask(true)
		GL.glPopState(GL_BLEND)
	}

	def setPointOfView(pov: TCamera) {}

	def update(f: Float) {
		val pov = graphicsEngine.pov
		if ( radius != pov.viewDistance ) {
			radius = pov.viewDistance
			var vi = 0
			var ii = 0

			val w = 10
			val h = 10
			vbo.numPoints = (w+1)*(h+1) * 6
			vbo.numIndices = (w*h) * 6 * 6

			def pointIndex (q : Int, x : Int, y : Int) = q * (w+1) * (h+1) + x * (h+1) + y
			for ( q <- 0 until 6 ; x <- 0 to w ; y <- 0 to h ; mult = if ( q < 3 ) { 1.0f } else { -1.0f } ; xf = x.toFloat/w.toFloat * mult ; yf = y.toFloat/h.toFloat * mult ) {
				var v = if ( q < 3 ) { Vec3f(-0.5f,-0.5f,-0.5f) } else { Vec3f(0.5f,0.5f,0.5f) }
				val index = pointIndex(q,x,y)
				if ( q % 3 == 0 ) { v.x += xf; v.y += yf; }
				else if ( q % 3 == 1 ) { v.x += xf; v.z += yf; }
				else if ( q % 3 == 2 ) { v.y += xf; v.z += yf; }
				v = functions.normalize(v) * (radius-2.0f)//make it a sphere
				vbo.setA(SimpleAttributeProfile.VertexAttribute,index,v)
				vbo.setAbf(SimpleAttributeProfile.ColorAttribute,index,x.toFloat / w.toFloat,y.toFloat / h.toFloat,q.toFloat / 6.0f,1.0f,128)
				vbo.setA(SimpleAttributeProfile.TexCoordAttribute,index,0.0f,0.0f)

				var forward = q < 3
				if ( q % 3 == 1 ) { forward = ! forward; }
				if ( x < w && y < h ) {
					if ( forward ) { //for face culling
						vbo.setI(ii + 0,index)
						vbo.setI(ii + 1,pointIndex(q,x+1,y))
						vbo.setI(ii + 2,pointIndex(q,x+1,y+1))

						vbo.setI(ii + 3,pointIndex(q,x+1,y+1))
						vbo.setI(ii + 4,pointIndex(q,x,y+1))
						vbo.setI(ii + 5,index)
					} else {
						vbo.setI(ii + 5,index)
						vbo.setI(ii + 4,pointIndex(q,x+1,y))
						vbo.setI(ii + 3,pointIndex(q,x+1,y+1))

						vbo.setI(ii + 2,pointIndex(q,x+1,y+1))
						vbo.setI(ii + 1,pointIndex(q,x,y+1))
						vbo.setI(ii + 0,index)
					}
					vi += 4
					ii += 6
				}
			}

			vbo.lastUpdatedMarker += 1
			vbo.state.set(AVBO.Updated)
		}
	}
}