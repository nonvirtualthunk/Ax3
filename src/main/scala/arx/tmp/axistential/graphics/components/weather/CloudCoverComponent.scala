package arx.axistential.graphics.components.weather

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/9/13
 * Time: 5:14 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.axistential.game.data.world.CloudData
import arx.axistential.game.data.world.ScaleData
import arx.axistential.graphics.shader.CloudShader
import arx.core.gen.ArxGenerators
import arx.core.gen.SimplexNoise
import arx.core.units.UnitOfTime
import arx.core.vec.Vec3f
import arx.core.vec.Vec4i
import arx.graphics._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15

import scalaxy.loops._

trait TCloudGraphicsComponent extends GraphicsComponent {
	def cloudTexture : Texture
}

class CloudCoverComponent extends GraphicsComponent with TCloudGraphicsComponent {
	override def drawOrder = GraphicsComponentDrawOrder.AbsolutelyFinal * 10

	val profile = new AttributeProfile( List("Vertex" -> (3,GL_FLOAT),"Color" -> (2,GL_FLOAT)) )

	val overCloudVBO = new AVBO(profile)
	val underCloudVBO = new AVBO(profile)

	lazy val shader = new CloudShader(world,pov)

	lazy val size = pov.viewDistance + 32.0f
	var height = 175.0f
	val layers = 3
	val layerGap = 10.0f

	override def initialize() {
		val cloudData = world.auxData[CloudData]

		val worldScale = world.aux[ScaleData]
		height = worldScale.CloudLevel.inVoxels

		cloudTexture = Texture.fromImage(cloudData.cloudImage)
		cloudTexture.mipmap = true
		cloudTexture.magFilter = GL_LINEAR
		cloudTexture.minFilter = GL_LINEAR_MIPMAP_LINEAR
		cloudTexture.wrap = GL_REPEAT

		val VertexAttr = profile.attributesByName("Vertex")
		val ColorAttr = profile.attributesByName("Color")

		for ( (cloudVBO,inverted) <- List(underCloudVBO -> true,overCloudVBO -> false) ) {
			val split = 10
			val invSplit = 1.0f / split
			val size2 = size * 2.0f

			val halfLayers = layers / 2

			val start = if ( inverted ) { layers - 1 } else { 0 }
			val end = if ( inverted ) { 0 } else { layers - 1 }
			val delta = if ( inverted ) { -1 } else { 1 }
			for ( l <- start to end by delta ) {
				val startVI = cloudVBO.incrementVertexOffset((split+1) * (split+1))
				var vi = startVI
				for ( x <- 0 to split optimized ; y <- 0 to split optimized ) {
					val xf = x * invSplit
					val yf = y * invSplit
					val xg = xf + invSplit
					val yg = yf + invSplit

					def dist ( tx : Float , ty : Float ) = ((0.5f - tx) * (0.5f - tx) + (0.5f - ty) * (0.5f - ty)) * 400.0f
//					val ii = cloudVBO.incrementIndexOffset(6)

					val layerWeight  = powf(0.55f,(l - halfLayers).abs + 1)
					val lightness = 0.7f + (l.toFloat / layers.toFloat) * 1.0f

					def offsetZ ( z : Float ) = floorf(z) + 0.15f
					cloudVBO.setA( VertexAttr , vi + 0 , floorf(-size + size2*xf), floorf(-size + size2*yf) , offsetZ(height + l*layerGap - dist(xf,yf)) )
//					cloudVBO.setA( VertexAttr , vi + 1 , floorf(-size + size2*xg), floorf(-size + size2*yf) , offsetZ(height +  l*layerGap - dist(xg,yf)) )
//					cloudVBO.setA( VertexAttr , vi + 2 , floorf(-size + size2*xg), floorf(-size + size2*yg) , offsetZ(height +   l*layerGap - dist(xg,yg)) )
//					cloudVBO.setA( VertexAttr , vi + 3 , floorf(-size + size2*xf), floorf(-size + size2*yg) , offsetZ(height +  l*layerGap - dist(xf,yg)) )

					cloudVBO.setA( ColorAttr  , vi + 0 , lightness, layerWeight )
//					cloudVBO.setA( ColorAttr  , vi + 1 , lightness, layerWeight )
//					cloudVBO.setA( ColorAttr  , vi + 2 , lightness, layerWeight )
//					cloudVBO.setA( ColorAttr  , vi + 3 , lightness, layerWeight )

					vi += 1
				}

				var ii = cloudVBO.incrementIndexOffset(6 * split * split)
				def vIndex ( x : Int , y : Int ) = startVI + x * (split+1) + y

				for ( x <- 0 until split optimized ; y <- 0 until split optimized ) {
					val x1 = x + 1
					val y1 = y + 1

					cloudVBO.setI(ii+0,vIndex(x,y))
					cloudVBO.setI(ii+1,vIndex(x1,y))
					cloudVBO.setI(ii+2,vIndex(x1,y1))

					cloudVBO.setI(ii+3,vIndex(x1,y1))
					cloudVBO.setI(ii+4,vIndex(x,y1))
					cloudVBO.setI(ii+5,vIndex(x,y))
					ii += 6
				}
			}


		}
	}


	var cloudTexture : Texture = null

	def draw(graphicsContext: RenderingContext) {
		if ( (pov.eye.z < height && pov.forward.z > -0.35f) || pov.eye.z >= height ) {
			shader.bind()

			val cloudVBO = if ( pov.eye.z < height ) { underCloudVBO } else { overCloudVBO }
			if ( ! cloudVBO.isSolidified ) { cloudVBO.solidify(GL15.GL_STATIC_DRAW) }
			val cloudData = world.auxData[CloudData]
			cloudTexture.bind(1)

			val povCopy = new AnthologiconCamera()
			povCopy.eye = Vec3f(0.0f,0.0f,pov.eye.z)
			povCopy._forward = pov.forward
			povCopy._up = pov.up
			povCopy.viewDistance = pov.viewDistance
			povCopy.near = pov.near

			povCopy.look()

			GL.glSetState(GL_CULL_FACE,enable=false)
			GL.glSetState(GL_DEPTH_TEST,enable=true)
//			GL.glDepthMask(enable = false)

			cloudVBO.drawElements(GL_TRIANGLES,0,-1)

//			GL.glDepthMask(enable = true)
		}

	}
	def setPointOfView(pov: TCamera) {}
	protected def update(f: Float) {
		//do nothing...no updates required
	}
}

class CloudGameEngineComponent extends GameEngineComponent {


	override def initialize() {
		val cloudData = env.auxData[CloudData]

		val W = 1024
		val W_2 = W >> 1
		val R = sqrtf(W_2*W_2+W_2*W_2)
		cloudData.cloudImage = Image.withDimensions(W,W)

		import ArxGenerators._
		val turb = Turbulence( Scale(0.01f) >> Simplex(new SimplexNoise(System.currentTimeMillis() + 10)) >> Mult(10.0f) )
		val gen = turb >> Scale(0.0045f) >> Fractal(4,1.6123f)( Simplex(new SimplexNoise(System.currentTimeMillis())) ) >> Mult(0.35f) >> Add(0.7f) >> Pow(2.0f)
		cloudData.cloudImage.setPixelsFromFunc( (x:Int,y:Int) => {
			val dx = W_2 - x
			val dy = W_2 - y
			var distFactor = sqrtf(dx * dx + dy * dy) / (W_2).toFloat
			if ( distFactor > 0.85f ) { distFactor = (distFactor - 0.85f) * 3.0f }
			else { distFactor = 0.0f }


			val b = gen(x,y)
			val c = 0.1f + b * 0.9f
			val d = c + distFactor

			val fb = d.clamp(0.0f,1.0f)
			val f = fb
			Vec4i((f*255).toInt,(f*255).toInt,(f*255).toInt,255)
		//		Vec4i(255,255,255,255)
		})
		Image.save(cloudData.cloudImage,"/tmp/cloudImage.png")
	}

	def update(time: UnitOfTime) {
		val cloudData = env.auxData[CloudData]

		cloudData.cloudOffset += time.inSeconds * 2.0f
	}
}
