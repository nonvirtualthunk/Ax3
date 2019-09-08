package arx.samvival.graphics.components

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/18/18
  * Time: 8:42 AM
  */

import arx.Prelude._
import arx.core.math.Recti
import scalaxy.loops._
import arx.core.vec._
import arx.core.vec.coordinates.{AxialVec, CartVec, CartVec3, VoxelCoord}
import arx.engine.advanced.lenginepieces.LGraphicsEngine
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.{CustomCanvasGraphicsComponent, LCustomCanvasGraphicsComponent}
import arx.engine.lworld.GameEventClock
import arx.engine.simple.{CustomCanvas, QuadBuilder}
import arx.graphics.helpers.HSBA
import arx.graphics._
import arx.resource.ResourceManager
import org.lwjgl.opengl.GL11
import org.lwjgl.opengl.GL11.{GL_FLOAT, GL_UNSIGNED_BYTE}
import SamvivalGraphicsComponent._
import arx.engine.advanced.lenginecomponents.LGraphicsComponent

trait SamvivalGraphicsComponent extends LGraphicsComponent {
	def pixelToHex(pixel : ReadVec2f, viewport : Recti) = {
		val unprojected = pov.unproject(Vec3f(pixel,0.0f), viewport)
		AxialVec.fromCartesian(unprojected.xy, SamvivalGraphicsComponent.HexSize)
	}
	def hexToPixel(hex : AxialVec, viewport : Recti) = {
		val px = hex.cartesianX(HexSize)
		val py = hex.cartesianY(HexSize)

		Vec2f(px + pov.eye.x + (viewport.width >> 1), py + pov.eye.y + (viewport.height >> 1))
	}
}

abstract class SamvivalCanvasGraphicsComponent(engine: LGraphicsEngine) extends LCustomCanvasGraphicsComponent[SVCanvas](engine, new SVCanvas(SamvivalGraphicsComponent.HexSize)) with SamvivalGraphicsComponent{

	canvas.useTexFilters(GL11.GL_NEAREST, GL11.GL_NEAREST)
	canvas.useHighCapacity(true)

//	canvas.viewportOverride = Some((r) => Recti(r.x,r.y+200,r.w,r.h-200))

	shader = ResourceManager.shader("samvival/shaders/SamvivalMain")
	shader.name = "SamvivalMainShader"
	depthTest = true
	depthFunc = GL11.GL_LEQUAL

	var fogTexture = Texture.fromImage(ResourceManager.image("samvival/materials/fog.png"))

	var lastDrawnGameTime = GameEventClock(-1)

	override def needsUpdate = true //lastDrawnGameTime < world.currentTime


	override def updateStarted(): Unit = {

	}

	override def updateComplete(): Unit = {

	}

	override def preDraw(): Unit = {
		shader.setUniform("fogTexture", 1)

		fogTexture.bind(1)
	}
}

object SamvivalGraphicsComponent {
	val HexSize = 124
}

class SVCanvas(val hexScale : Int) extends CustomCanvas[SVQuadBuilder](SVAttributeProfile) {
	override def createQuadBuilder(): SVQuadBuilder = new SVQuadBuilder(vbo, textureBlock, blankTC, hexScale)


	def line(startCV3 : CartVec3, endCV3 : CartVec3, layer : Int, hexThickness : Float, color : HSBA) = {
		val start = startCV3.xy
		val end = endCV3.xy
		val thickness = hexThickness * hexScale * 2.0f

		val middle = (start + end) * 0.5f
		val delta = (end - start).normalizeSafe
		val normal = functions.cross(Vec3f(delta,0.0f), Vec3f(0.0f, 0.0f, 1.0f))

		quad(middle, layer)
			.withDimensions((end - start).lengthSafe * hexScale, thickness)
			.withOrtho(normal)
			.withForward(Vec3f(delta, 0.0f))
			.withTexture(ResourceManager.blankImage)
			.withColor(color)
	}

	def quad(hex : AxialVec, layer : Int) = createQuadBuilder().withPosition(hex, Vec2f.Zero, layer)
//	def quad(pos : ReadVec2f, layer : Int) = createQuadBuilder().withPosition(pos.x,pos.y,layer)
	def quad(pos : CartVec, layer : Int) = createQuadBuilder().withPosition(pos.x * hexScale,pos.y * hexScale,layer)
	def quad(pos : CartVec, layer : Int, zNudge : Float) = createQuadBuilder().withPosition(pos.x * hexScale,pos.y * hexScale,layer.toFloat + zNudge)
	def quad(pos : CartVec3, layer : Int) = createQuadBuilder().withPosition(pos.x * hexScale,pos.y * hexScale,layer)
	def quad(pos : CartVec3, layer : Int, zNudge : Float) = createQuadBuilder().withPosition(pos.x * hexScale,pos.y * hexScale,layer.toFloat + zNudge)
}

object SVAttributeProfile extends AttributeProfile(List("vertex" -> (3, GL_FLOAT), "texCoord" -> (2, GL_FLOAT), "color" -> (4, GL_UNSIGNED_BYTE), "visionPcnt" -> (1, GL_FLOAT), "lightColor" -> (4,GL_UNSIGNED_BYTE))) {
	val VertexAttribute = attributesByName("vertex")
	val TexCoordAttribute = attributesByName("texCoord")
	val ColorAttribute = attributesByName("color")
	val VisionAttribute = attributesByName("visionPcnt")
	val LightColorAttribute = attributesByName("lightColor")
}


class SVQuadBuilder(vbo: AVBO, textureBlock: TextureBlock, blankTC: Array[ReadVec2f], val hexScale : Int) extends QuadBuilder(vbo, textureBlock, blankTC) {
	var visionPcnt = 1.0f
	var lr = 1.0f
	var lg = 1.0f
	var lb = 1.0f
	var centered = true

	override def withColor(hsba: ReadVec4f): SVQuadBuilder.this.type = {
		if (! hsba.isInstanceOf[HSBA]) {
			throw new UnsupportedOperationException("only use hsba colors")
		} else {
			super.withColor(hsba)
		}
	}

	def withDimensions(v : CartVec) = {
		super.withDimensions(v * hexScale)
	}
	def withSizeInHexes(f : Float) = {
		super.withDimensions(f * hexScale, f * hexScale)
	}

	def withCentered(b : Boolean) = {
		centered = b
		this
	}

	def withOffsetPixels(p : ReadVec2f) = {
		position = position + Vec3f(p,0.0f)
		this
	}

	def withPosition(hex : AxialVec, offset : ReadVec2f, layer : Int) = {
		position = Vec3f((hex.asCartesian + offset) * hexScale, layer)
		this
	}

	def withVisionPcnt(v: Float) = {
		visionPcnt = v
		this
	}

	def withLightColor(c : ReadVec3f) : this.type = {
		lr = c.r
		lg = c.g
		lb = c.b
		this
	}

	def withLightColor(r:Float,g:Float,b:Float) : this.type = {
		lr = r
		lg = g
		lb = b
		this
	}

	def withForward2D(forward : ReadVec3f) = {
		val normed = forward.normalizeSafe
		val ortho = normed.cross(Vec3f.UnitZ)
		this.withForward(normed)
   		.withOrtho(ortho)
	}

	override def draw(): Unit = {
		val vi = vbo.incrementVertexOffset(4)
		val ii = vbo.incrementIndexOffset(6)

		var i = 0

		while (i < 4) {
			if (centered) {
				val x = position.x + forward.x * (Cardinals.centeredCubePoints(Cardinals.Top)(i).x * dimensions.x) + ortho.x * (Cardinals.centeredCubePoints(Cardinals.Top)(i).y * dimensions.y)
				val y = position.y + forward.y * (Cardinals.centeredCubePoints(Cardinals.Top)(i).x * dimensions.x) + ortho.y * (Cardinals.centeredCubePoints(Cardinals.Top)(i).y * dimensions.y)
//				val z = position.z + forward.z * (Cardinals.centeredCubePoints(Cardinals.Top)(i).x * dimensions.x) + ortho.z * (Cardinals.centeredCubePoints(Cardinals.Top)(i).y * dimensions.y)\
				// conceptual "z" is actually based on the central y value with z being used as a nudge
				// Tiles are treated separately, but...probably the other things could use this, though we may need the ability to override
				val z = if (position.z <= 0) {
					-99
				} else {
					-position.y * 0.1f + position.z * 0.01f
				}
				vbo.setA(SVAttributeProfile.VertexAttribute, vi + i, x, y,z)
			} else {
				val x = position.x + forward.x * (Cardinals.cubePoints(Cardinals.Top)(i).x * dimensions.x) + ortho.x * (Cardinals.cubePoints(Cardinals.Top)(i).y * dimensions.y)
				val y = position.y + forward.y * (Cardinals.cubePoints(Cardinals.Top)(i).x * dimensions.x) + ortho.y * (Cardinals.cubePoints(Cardinals.Top)(i).y * dimensions.y)

				val z = if (position.z <= 0) {
					-99
				} else {
					-position.y * 0.1f + position.z * 0.01f
				}
				vbo.setA(SVAttributeProfile.VertexAttribute, vi + i, x, y,z)
			}
			vbo.setAbf(SVAttributeProfile.ColorAttribute, vi + i, color.r, color.g, color.b, color.a, 255)
			vbo.setA(SVAttributeProfile.TexCoordAttribute, vi + i, texCoords((i + textureIndexRotation) % 4))
			vbo.setA(SVAttributeProfile.VisionAttribute, vi + i, visionPcnt)
			vbo.setAbf(SVAttributeProfile.LightColorAttribute, vi + i, lr, lg, lb, 1.0f, 128)
			i += 1
		}
		vbo.setIQuad(ii, vi)
	}
}