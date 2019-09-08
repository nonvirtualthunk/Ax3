package arx.tyche.graphics.components

import arx.Prelude.{cosf, sinf, toRad}
import arx.application.Noto
import arx.core.math.Rectf
import arx.core.vec.{Cardinals, ReadVec2f, ReadVec3f, ReadVec4f, Vec2f, Vec3f, Vec4f}
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.{CanvasGraphicsComponent, CustomCanvasGraphicsComponent, GraphicsComponent}
import arx.engine.simple.{Canvas, CustomCanvas, TQuadBuilder}
import arx.graphics.{AVBO, Image, TextureBlock}
import arx.graphics.attributeprofiles.SimpleAttributeProfile
import arx.resource.ResourceManager
import arx.tyche.core.GlobeCoord
import arx.tyche.graphics.core.TyAttributeProfile
import org.lwjgl.opengl.GL11
import org.lwjgl.opengl.GL11.{GL_BLEND, GL_CULL_FACE, GL_DEPTH_TEST}

//abstract class TyGraphicsComponent(ge : GraphicsEngine) extends GraphicsComponent(ge) {
//	val vbo = new AVBO(TyAttributeProfile)
//	var shader = ResourceManager.shader("tyche/shaders/TycheMain")
//
//	override def draw(): Unit = {
//
//		arx.graphics.GL.glSetState(GL_CULL_FACE, enable = true)
//		arx.graphics.GL.glSetState(GL_DEPTH_TEST, enable = true)
//		arx.graphics.GL.glSetDepthFunc(GL11.GL_LESS)
//		arx.graphics.GL.glSetState(GL_BLEND, enable = true)
//
//		shader.bind()
//
//		pov.resolve().look()
//
//		vbo.bind()
//		vbo.solidifyIfNecessary()
//		vbo.drawElements()
//	}
//}


abstract class TyGraphicsComponent(ge: GraphicsEngine) extends CustomCanvasGraphicsComponent[TyCanvas](ge) {
	shader = ResourceManager.shader("tyche/shaders/TycheMain")
	shader.name = "TycheMain"
	depthTest = true
	depthFunc = GL11.GL_LEQUAL

	canvas.eye = () => pov.resolve().eye

}

class TyCanvas extends CustomCanvas[TyQuadBuilder](TyAttributeProfile) {
	var eye : () => ReadVec3f = () => Vec3f.Zero

	override def createQuadBuilder(): TyQuadBuilder = new TyQuadBuilder(vbo, textureBlock, blankTC, eye())

	def tri(p1: GlobeCoord, p2: GlobeCoord, p3: GlobeCoord): TyTriBuilder = {
		new TyTriBuilder(vbo, textureBlock, blankTC).withPoints(p1, p2, p3)
	}

	def tri(p: Seq[GlobeCoord]): TyTriBuilder = {
		new TyTriBuilder(vbo, textureBlock, blankTC).withPoints(p)
	}

	def quad(p : GlobeCoord) : TyQuadBuilder = createQuadBuilder().withPosition(p)
}

sealed trait TyOrientation {}

object TyOrientation {

	object Surface extends TyOrientation

	object Standing extends TyOrientation

	object Billboard extends TyOrientation
}

abstract class TyPolyBuilder(vbo: AVBO, textureBlock: TextureBlock, blankTC: Array[ReadVec2f]) {
	var color = Vec4f.One
	var texCoords = blankTC
	var textureIndexRotation = 0
	var orientation: TyOrientation = TyOrientation.Surface

	def withColor(r: Float, g: Float, b: Float, a: Float): this.type = {
		color = Vec4f(r, g, b, a)
		this
	}

	def withColor(rgba: ReadVec4f): this.type = {
		color = rgba
		this
	}

	def withTexture(image: Image): this.type = {
		texCoords = textureBlock(image)
		this
	}

	def withTexture(imageStr: String): this.type = {
		texCoords = textureBlock(ResourceManager.image(imageStr))
		this
	}

	def withSubTexture(image: Image, pcntRect: Rectf): this.type = {
		val fullRect = textureBlock.getOrElseUpdateRectFor(image)
		val effRect = Rectf(fullRect.x + fullRect.w * pcntRect.x, fullRect.y + fullRect.h * pcntRect.y,
			fullRect.w * pcntRect.w, fullRect.h * pcntRect.h)
		withTexCoords(effRect)
	}

	def withTexCoords(effRect: Rectf): this.type = {
		texCoords = Array(effRect.xy, Vec2f(effRect.maxX, effRect.y), Vec2f(effRect.maxX, effRect.maxY), Vec2f(effRect.x, effRect.maxY))
		this
	}

	def withTexCoords(tc: Array[ReadVec2f]): this.type = {
		texCoords = tc
		this
	}

	def withTextureIndexRotation(r: Int): this.type = {
		textureIndexRotation = r
		this
	}
}

class TyTriBuilder(vbo: AVBO, textureBlock: TextureBlock, blankTC: Array[ReadVec2f]) extends TyPolyBuilder(vbo, textureBlock, blankTC) {
	val points = Array.ofDim[GlobeCoord](3)
	val normals = Array.ofDim[ReadVec3f](3)
	val lightColors = Array.ofDim[ReadVec4f](3)
	val lightPcnts = Array.ofDim[Float](3)

	def withPoints(p1: GlobeCoord, p2: GlobeCoord, p3: GlobeCoord): this.type = {
		points(0) = p1
		points(1) = p2
		points(2) = p3
		this
	}

	def withNormals(n1: ReadVec3f, n2: ReadVec3f, n3: ReadVec3f): this.type = {
		normals(0) = n1
		normals(1) = n2
		normals(2) = n3
		this
	}

	def withLighting(lc1 : ReadVec4f, lp1 : Float,
						  lc2 : ReadVec4f, lp2 : Float,
						  lc3 : ReadVec4f, lp3 : Float) : this.type = {
		lightColors(0) = lc1
		lightColors(1) = lc2
		lightColors(2) = lc3

		lightPcnts(0) = lp1
		lightPcnts(1) = lp2
		lightPcnts(2) = lp3
		this
	}

	def withPoints(p: Seq[GlobeCoord]): this.type = {
		if (p.size >= 3) {
			points(0) = p(0)
			points(1) = p(1)
			points(2) = p(2)
		} else {
			Noto.error("withPoints(...) called with invalid sized sequence")
		}
		this
	}

	def draw(): Unit = {
		val vi = vbo.incrementVertexOffset(3)
		val ii = vbo.incrementIndexOffset(3)

		var i = 0
		while (i < 3) {
			val cartV = points(i).asCartesian
			val normal = normals(i)
			vbo.setA(TyAttributeProfile.VertexAttribute, vi + i, cartV)

			vbo.setAbf(TyAttributeProfile.ColorAttribute, vi + i, color.r, color.g, color.b, color.a, 255)
			vbo.setA(TyAttributeProfile.TexCoordAttribute, vi + i, texCoords((i + textureIndexRotation) % 4))
//			vbo.setAbf(TyAttributeProfile.Normal, vi + i, normal.x, normal.y, normal.z, 128)
			vbo.setA(TyAttributeProfile.Normal, vi + i, normal)
			vbo.setA(TyAttributeProfile.BillboardAttribute, vi + i, 0.0f, 0.0f)
			vbo.setA(TyAttributeProfile.LightPcntAttribute, vi + i, lightPcnts(i))
			if (lightColors(i) != null) {
				vbo.setAbf(TyAttributeProfile.LightColorAttribute, vi + i, lightColors(i).r, lightColors(i).g, lightColors(i).b, lightColors(i).a, 255)
			} else {
				vbo.setAbf(TyAttributeProfile.LightColorAttribute, vi + i, 1.0f,1.0f,1.0f,1.0f, 255)
			}
			vbo.setI(ii + i, vi + i)
			i += 1
		}
	}
}

class TyQuadBuilder(vbo: AVBO, textureBlock: TextureBlock, blankTC: Array[ReadVec2f], eye : ReadVec3f) extends TyPolyBuilder(vbo, textureBlock, blankTC) with TQuadBuilder {
	var position: GlobeCoord = GlobeCoord()
	var cartPosition: Vec3f = Vec3f.Zero
	var dimensions = Vec2f.One
	var normal: ReadVec3f = Vec3f.UnitZ
	var lightPcnt : Float = 0.0f
	var lightColor : ReadVec4f = Vec4f.One

	def withNormal(n: ReadVec3f): this.type = {
		normal = n
		this
	}

	def withPosition(gc: GlobeCoord): this.type = {
		position = gc
		this
	}

	def withCartPosition(v: ReadVec3f): this.type = {
		cartPosition = v
		this
	}

	def withDimensions(s: Float): this.type = {
		dimensions = Vec2f(s, s)
		this
	}

	def withDimensions(x: Float, y: Float): this.type = {
		dimensions = Vec2f(x, y)
		this
	}

	def withDimensions(d: ReadVec2f): this.type = {
		dimensions = d
		this
	}

	def withOrientation(o: TyOrientation): this.type = {
		orientation = o
		this
	}

	def withLighting(lightColor : ReadVec4f, lightPcnt : Float) : this.type = {
		this.lightColor = lightColor
		this.lightPcnt = lightPcnt
		this
	}

	val quadPoints = Array(
		Vec2f(-0.5f, 0.0f),
		Vec2f(0.5f, 0.0f),
		Vec2f(0.5f, 1.0f),
		Vec2f(-0.5f, 1.0f)
	)

	def draw(): Unit = {
		val vi = vbo.incrementVertexOffset(4)
		val ii = vbo.incrementIndexOffset(6)

		var i = 0
		while (i < 4) {
			orientation match {
				case TyOrientation.Surface =>
					val center = if (cartPosition != Vec3f.Zero) { cartPosition } else { position.asCartesian }
					//					val center = cartPosition
					val ortho = center.normalizeSafe.cross(Vec3f.UnitZ).normalizeSafe
					val forward = center.normalizeSafe.cross(ortho).normalizeSafe
					val x = center.x + forward.x * (Cardinals.centeredCubePoints(Cardinals.Top)(i).x * dimensions.x) + ortho.x * (Cardinals.centeredCubePoints(Cardinals.Top)(i).y * dimensions.y)
					val y = center.y + forward.y * (Cardinals.centeredCubePoints(Cardinals.Top)(i).x * dimensions.x) + ortho.y * (Cardinals.centeredCubePoints(Cardinals.Top)(i).y * dimensions.y)
					val z = center.z + forward.z * (Cardinals.centeredCubePoints(Cardinals.Top)(i).x * dimensions.x) + ortho.z * (Cardinals.centeredCubePoints(Cardinals.Top)(i).y * dimensions.y)
					vbo.setA(TyAttributeProfile.VertexAttribute, vi + i, x, y, z)
					vbo.setA(TyAttributeProfile.BillboardAttribute, vi + i, 0.0f,0.0f)
				case TyOrientation.Standing =>
//					val center = position.asCartesian
//					val ortho = center.normalize * 0.5f
////					val forward = ortho.cross(Vec3f.UnitZ)
////					val ortho = Vec3f.Zero
//					val forward = Vec3f.Zero
////					val center = rawCenter + ortho * dimensions.y * 0.5f
//					val x = center.x + forward.x * (quadPoints(i).x * dimensions.x) + ortho.x * (quadPoints(i).y * dimensions.y)
//					val y = center.y + forward.y * (quadPoints(i).x * dimensions.x) + ortho.y * (quadPoints(i).y * dimensions.y)
//					val z = center.z + forward.z * (quadPoints(i).x * dimensions.x) + ortho.z * (quadPoints(i).y * dimensions.y)
//					vbo.setA(TyAttributeProfile.VertexAttribute, vi + i, x, y, z)
//					vbo.setA(TyAttributeProfile.BillboardAttribute, vi + i,
//						quadPoints(i).x * dimensions.x,
//						quadPoints(i).y * dimensions.y * 0.5f)


					val basePoint = if (cartPosition != Vec3f.Zero) { cartPosition } else { position.asCartesian } // origin point on the edge of the sphere (probably)
					val vectorToEye = (eye).normalizeSafe // from origin to the eye
					val rawUp = basePoint.normalizeSafe // up, if you don't account for the viewing angle
					val ortho = rawUp.cross(vectorToEye).normalizeSafe // ortho, relative to the eye vector
					val modifiedUp = vectorToEye.cross(ortho).normalizeSafe

					val up = ((rawUp + modifiedUp) * 0.5f).normalizeSafe


					val x = basePoint.x + ortho.x * (quadPoints(i).x * dimensions.x) + up.x * (quadPoints(i).y * dimensions.y)
					val y = basePoint.y + ortho.y * (quadPoints(i).x * dimensions.x) + up.y * (quadPoints(i).y * dimensions.y)
					val z = basePoint.z + ortho.z * (quadPoints(i).x * dimensions.x) + up.z * (quadPoints(i).y * dimensions.y)
					vbo.setA(TyAttributeProfile.VertexAttribute, vi + i, x, y, z)
					vbo.setA(TyAttributeProfile.BillboardAttribute, vi + i, 0.0f,0.0f)
//						quadPoints(i).x * dimensions.x,
//						quadPoints(i).y * dimensions.y * 0.5f)

				case TyOrientation.Billboard =>
					val center = if (cartPosition != Vec3f.Zero) { cartPosition } else { position.asCartesian }

					vbo.setA(TyAttributeProfile.VertexAttribute, vi + i, center)
					vbo.setA(TyAttributeProfile.BillboardAttribute, vi + i, quadPoints(i).x * dimensions.x, quadPoints(i).y * dimensions.y)
			}
			vbo.setAbf(TyAttributeProfile.ColorAttribute, vi + i, color.r, color.g, color.b, color.a, 255)
			vbo.setAbf(TyAttributeProfile.LightColorAttribute, vi + i, lightColor.r, lightColor.g, lightColor.b, lightColor.a, 255)
			vbo.setA(TyAttributeProfile.LightPcntAttribute, vi + i, lightPcnt)
			vbo.setA(TyAttributeProfile.TexCoordAttribute, vi + i, texCoords((i + textureIndexRotation) % 4))
			vbo.setA(TyAttributeProfile.Normal, vi + i, normal.x, normal.y, normal.z)
//			vbo.setAbf(TyAttributeProfile.Normal, vi + i, normal.x, normal.y, normal.z, 128)
			i += 1
		}
		vbo.setIQuad(ii, vi)
	}

}