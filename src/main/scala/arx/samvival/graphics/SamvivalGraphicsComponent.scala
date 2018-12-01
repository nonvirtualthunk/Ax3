package arx.samvival.graphics

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
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.advanced.lenginepieces.LGraphicsEngine
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.{CustomCanvasGraphicsComponent, LCustomCanvasGraphicsComponent}
import arx.engine.lworld.GameEventClock
import arx.engine.simple.{CustomCanvas, QuadBuilder}
import arx.graphics.helpers.HSBA
import arx.graphics.{AVBO, AttributeProfile, Texture, TextureBlock}
import arx.resource.ResourceManager
import org.lwjgl.opengl.GL11
import org.lwjgl.opengl.GL11.{GL_FLOAT, GL_UNSIGNED_BYTE}

abstract class SamvivalGraphicsComponent(engine: LGraphicsEngine) extends LCustomCanvasGraphicsComponent[SVCanvas](engine) {
	canvas.useTexFilters(GL11.GL_NEAREST, GL11.GL_NEAREST)
	canvas.useHighCapacity(true)

	canvas.viewportOverride = Some((r) => Recti(r.x,r.y+200,r.w,r.h-200))

	shader = ResourceManager.shader("samvival/shaders/SamvivalMain")
	shader.name = "SamvivalMainShader"
	depthTest = true

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

class SVCanvas extends CustomCanvas[SVQuadBuilder](SVAttributeProfile) {
	override def createQuadBuilder(): SVQuadBuilder = new SVQuadBuilder(vbo, textureBlock, blankTC)

	def line(start: ReadVec2f, end: ReadVec2f, thickness: Float, color: ReadVec4f) = {
		val middle = (start + end) * 0.5f
		val delta = (end - start).normalizeSafe
		val normal = functions.cross(Vec3f(delta, 0.0f), Vec3f(0.0f, 0.0f, 1.0f))

		createQuadBuilder()
			.withPosition(middle)
			.withDimensions((end - start).lengthSafe, thickness)
			.withOrtho(normal)
			.withForward(Vec3f(delta, 0.0f))
			.withTexture(ResourceManager.blankImage)
			.withColor(color)
	}
}

object SVAttributeProfile extends AttributeProfile(List("vertex" -> (3, GL_FLOAT), "texCoord" -> (2, GL_FLOAT), "color" -> (4, GL_UNSIGNED_BYTE), "visionPcnt" -> (1, GL_FLOAT), "lightColor" -> (4,GL_UNSIGNED_BYTE))) {
	val VertexAttribute = attributesByName("vertex")
	val TexCoordAttribute = attributesByName("texCoord")
	val ColorAttribute = attributesByName("color")
	val VisionAttribute = attributesByName("visionPcnt")
	val LightColorAttribute = attributesByName("lightColor")
}


class SVQuadBuilder(vbo: AVBO, textureBlock: TextureBlock, blankTC: Array[ReadVec2f]) extends QuadBuilder(vbo, textureBlock, blankTC) {
	var visionPcnt = 1.0f
	var lr = 1.0f
	var lg = 1.0f
	var lb = 1.0f


	override def withColor(hsba: ReadVec4f): SVQuadBuilder.this.type = {
		if (! hsba.isInstanceOf[HSBA]) {
			throw new UnsupportedOperationException("only use hsba colors")
		} else {
			super.withColor(hsba)
		}
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

	override def draw(): Unit = {
		val vi = vbo.incrementVertexOffset(4)
		val ii = vbo.incrementIndexOffset(6)

		var i = 0
		while (i < 4) {
			if (cubeFace == -1) {
				val x = position.x + forward.x * (Cardinals.centeredCubePoints(Cardinals.Top)(i).x * dimensions.x) + ortho.x * (Cardinals.centeredCubePoints(Cardinals.Top)(i).y * dimensions.y)
				val y = position.y + forward.y * (Cardinals.centeredCubePoints(Cardinals.Top)(i).x * dimensions.x) + ortho.y * (Cardinals.centeredCubePoints(Cardinals.Top)(i).y * dimensions.y)
				val z = position.z + forward.z * (Cardinals.centeredCubePoints(Cardinals.Top)(i).x * dimensions.x) + ortho.z * (Cardinals.centeredCubePoints(Cardinals.Top)(i).y * dimensions.y)
				vbo.setA(SVAttributeProfile.VertexAttribute, vi + i, x, y, z)
			} else {

				val x = position.x + (Cardinals.centeredCubePoints(cubeFace)(i).x * dimensions.x)
				val y = position.y + (Cardinals.centeredCubePoints(cubeFace)(i).y * dimensions.y)
				val z = position.z + (Cardinals.centeredCubePoints(cubeFace)(i).z * dimZ)
				vbo.setA(SVAttributeProfile.VertexAttribute, vi + i, x, y, z)
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