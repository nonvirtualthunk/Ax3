package arx.rog2.graphics

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.math.Recti
import arx.core.vec.Cardinals
import arx.core.vec.ReadVec2f
import arx.core.vec.ReadVec3f
import arx.core.vec.ReadVec4f
import arx.core.vec.Vec3f
import arx.core.vec.Vec4f
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.functions
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.CustomCanvasGraphicsComponent
import arx.engine.simple.CustomCanvas
import arx.engine.simple.QuadBuilder
import arx.graphics.AVBO
import arx.graphics.AttributeProfile
import arx.graphics.Texture
import arx.graphics.TextureBlock
import arx.graphics.helpers.HSBA
import arx.resource.ResourceManager
import arx.rog2.engine.RogComponent
import arx.rog2.game.data.entity.Creature
import arx.rog2.game.data.entity.LightSource
import arx.rog2.game.data.entity.Physical
import arx.rog2.game.data.world.Light
import arx.rog2.game.data.world.Terrain
import org.lwjgl.opengl.GL11
import org.lwjgl.opengl.GL11.GL_FLOAT
import org.lwjgl.opengl.GL11.GL_UNSIGNED_BYTE

import scalaxy.loops.rangeExtensions

abstract class RogGraphicsComponent(engine: GraphicsEngine) extends CustomCanvasGraphicsComponent[RogCanvas](engine) with RogComponent {
	canvas.useTexFilters(GL11.GL_NEAREST, GL11.GL_NEAREST)
	canvas.useHighCapacity(true)

	canvas.viewportOverride = Some((r) => Recti(r.x,r.y+200,r.w,r.h-200))

	shader = ResourceManager.shader("rog/shaders/RogMap")
	shader.name = "RogMapShader"
	depthTest = true

	var fogTexture = Texture.fromImage(ResourceManager.image("rog/materials/fog.png"))

	var lastDrawnGameTime = -1.seconds

	override def needsUpdate = lastDrawnGameTime < world.time


	var lastUpdateStart = -1.seconds

	override def updateStarted(): Unit = {
		lastUpdateStart = world.time
	}

	override def updateComplete(): Unit = {
		lastDrawnGameTime = lastUpdateStart
	}

	override def preDraw(): Unit = {
		shader.setUniform("fogTexture", 1)

		fogTexture.bind(1)
	}

	class LightHolder(var lightPcnt : Float = 0.0f, var lightColor : Vec3f = new Vec3f) {
	}

	class LightAndVisionContext {
		val playerPosition = player[Physical].position
		val playerVision = player[Creature].visionGrid
		val playerVisionRange = player[Creature].sightRange

		val TD = world[Terrain]
		val LD = world[Light]


		val nearbyLights =
			LD.lightGrids.synchronized {
				LD.lightGrids.filter {
					case (ent, grid) =>
						ent.aux[Physical].position.distanceTo(playerPosition) < playerVisionRange + ent[LightSource].lightStrength &&
						ent.aux[Physical].heldIn.isEmpty
				}.toArray
			}

		val nearbyLightGrids = nearbyLights.map(_._2)
		val nearbyLightPositions = nearbyLights.map(_._1[Physical].position)
		val nearbyLightColors = nearbyLights.map(_._1[LightSource].lightColor)
		val nearbyLightHolders = Array.ofDim[Float](nearbyLights.length)
		val lightHolder = new LightHolder

		def noLight = {
			lightHolder.lightPcnt = 0.0f
			lightHolder.lightColor.r = 0.0f
			lightHolder.lightColor.g = 0.0f
			lightHolder.lightColor.b = 0.0f
			lightHolder
		}

		def lightPcntAt(v: VoxelCoord, q: Int) = {
			var maxLight = 0.0f
			var maxIndex = -1

			lightHolder.lightColor.r = 0.0f
			lightHolder.lightColor.g = 0.0f
			lightHolder.lightColor.b = 0.0f
			var lightSum = 0.0f

			for (i <- 0 until nearbyLightGrids.length optimized) {
				val p = nearbyLightPositions(i)
				val grid = nearbyLightGrids(i)
				val dx = v.x + Cardinals.cardinalsX(q) - p.x
				val dy = v.y + Cardinals.cardinalsY(q) - p.y
				val dz = v.z + Cardinals.cardinalsZ(q) - p.z

				if (dx.abs < 32 && dy.abs < 32 && dz.abs < 32) {
					val lv = grid(dx, dy, dz)
					nearbyLightHolders(i) = lv
					if (lv > maxLight) {
						maxLight = lv
						maxIndex = i
					}

					lightSum += lv
					val lcol = nearbyLightColors(i)
					lightHolder.lightColor.r += lcol.r * lv
					lightHolder.lightColor.g += lcol.g * lv
					lightHolder.lightColor.b += lcol.b * lv
				} else {
					nearbyLightHolders(i) = 0.0f
				}
			}

			var light = maxLight
			for (i <- 0 until nearbyLightHolders.length optimized) {
				if (i != maxIndex) {
					light += nearbyLightHolders(i) * 0.1f
				}
			}

//			if (lightSum > 0.0f) {
				lightHolder.lightColor.r /= lightSum
				lightHolder.lightColor.g /= lightSum
				lightHolder.lightColor.b /= lightSum
//			}

			lightHolder.lightPcnt = light.min(maxLight * 1.25f).min(1.0f)
			lightHolder
		}

		def visionPcntAt(v: VoxelCoord, q: Int) = {
			val dx = v.x + Cardinals.cardinalsX(q) - playerPosition.x
			val dy = v.y + Cardinals.cardinalsY(q) - playerPosition.y
			val dz = v.z + Cardinals.cardinalsZ(q) - playerPosition.z

			if (dx.abs < 32 && dy.abs < 32 && dz.abs < 32) {
				playerVision(dx, dy, dz)
			} else {
				0.0f
			}
		}
	}

}

class RogCanvas extends CustomCanvas[RogQuadBuilder](RogAttributeProfile) {
	override def createQuadBuilder(): RogQuadBuilder = new RogQuadBuilder(vbo, textureBlock, blankTC)

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

object RogAttributeProfile extends AttributeProfile(List("vertex" -> (3, GL_FLOAT), "texCoord" -> (2, GL_FLOAT), "color" -> (4, GL_UNSIGNED_BYTE), "visionPcnt" -> (1, GL_FLOAT), "lightColor" -> (4,GL_UNSIGNED_BYTE))) {
	val VertexAttribute = attributesByName("vertex")
	val TexCoordAttribute = attributesByName("texCoord")
	val ColorAttribute = attributesByName("color")
	val VisionAttribute = attributesByName("visionPcnt")
	val LightColorAttribute = attributesByName("lightColor")
}


class RogQuadBuilder(vbo: AVBO, textureBlock: TextureBlock, blankTC: Array[ReadVec2f]) extends QuadBuilder(vbo, textureBlock, blankTC) {
	var visionPcnt = 1.0f
	var lr = 1.0f
	var lg = 1.0f
	var lb = 1.0f


	override def withColor(hsba: ReadVec4f): RogQuadBuilder.this.type = {
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
				vbo.setA(RogAttributeProfile.VertexAttribute, vi + i, x, y, z)
			} else {

				val x = position.x + (Cardinals.centeredCubePoints(cubeFace)(i).x * dimensions.x)
				val y = position.y + (Cardinals.centeredCubePoints(cubeFace)(i).y * dimensions.y)
				val z = position.z + (Cardinals.centeredCubePoints(cubeFace)(i).z * dimZ)
				vbo.setA(RogAttributeProfile.VertexAttribute, vi + i, x, y, z)
			}
			vbo.setAbf(RogAttributeProfile.ColorAttribute, vi + i, color.r, color.g, color.b, color.a, 255)
			vbo.setA(RogAttributeProfile.TexCoordAttribute, vi + i, texCoords((i + textureIndexRotation) % 4))
			vbo.setA(RogAttributeProfile.VisionAttribute, vi + i, visionPcnt)
			vbo.setAbf(RogAttributeProfile.LightColorAttribute, vi + i, lr, lg, lb, 1.0f, 128)
			i += 1
		}
		vbo.setIQuad(ii, vi)
	}
}