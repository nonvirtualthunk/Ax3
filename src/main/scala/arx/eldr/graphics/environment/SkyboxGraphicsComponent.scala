package arx.eldr.graphics.environment

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.core.Moddable
import arx.core.units.UnitOfTime
import arx.eldr.graphics.data.FogUniformProvider
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.GraphicsComponent
import arx.graphics.attributeprofiles.SimpleAttributeProfile
import arx.graphics.pov.EyeCamera
import arx.graphics.pov.TCamera
import scalaxy.loops._

import arx.Prelude._
import org.lwjgl.opengl.{GL11, GL15}
import arx.graphics.{AVBO, GL, VBO}
import arx.resource.ResourceManager
import arx.core.vec.{functions, Vec3f}
import org.lwjgl.opengl.GL11._
import arx.application.Noto

class SkyboxGraphicsComponent(eng:GraphicsEngine) extends GraphicsComponent(eng) {
	name = "Skybox Component"
	var vbo = new AVBO(SimpleAttributeProfile)
	var radius = -1.0f

	val povCopy = new EyeCamera()

	lazy val fogComponent = new FogUniformProvider(world,Moddable(povCopy _))

//	isActive = () => settingValue[Boolean]("Graphics/Advanced/Draw Skybox")
//	override def initSettings = List(new BooleanSetting("Graphics/Advanced/Draw Skybox",true))

	drawPriority = 1000

	def draw() {
		GL.glPushState(GL_BLEND,truth = false)
		//		glDepthMask(false)

		val shader = ResourceManager.shader("eldr/shaders/SkyboxShader")
		shader.bind()
		fogComponent.setFogVariables(shader)

		GL.glSetState(GL11.GL_DEPTH_TEST,enable = true)
		GL.glSetState(GL11.GL_CULL_FACE,enable = true)
		GL.glSetCullFace(GL11.GL_BACK)

		povCopy.eye = Vec3f(0.0f,0.0f,0.0f)
		povCopy.forward = pov.forward
		povCopy.up = pov.up
		povCopy.viewDistance = pov.viewDistance
		povCopy.near = pov.near
		povCopy.fovy = pov.fovy

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

	def update(dt: UnitOfTime) {
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
			
			vbo.state.set(AVBO.Updated)
		}
	}
}