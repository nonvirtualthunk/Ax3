package arx.eldr.graphics.environment

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.ForwardingModdable
import arx.core.datastructures.voxel.Talea
import arx.core.datastructures.voxel.TaleaIterator
import arx.core.metrics.Metrics
import arx.core.units.UnitOfTime
import arx.core.vec.Cardinals
import arx.core.vec.coordinates.ConstVoxelCoord
import arx.core.vec.coordinates.TaleaCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.eldr.game.world.data.Light
import arx.eldr.game.world.data.Terrain
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.GraphicsComponent
import arx.graphics.AVBO
import arx.graphics.GL
import arx.graphics.TextureBlock
import arx.graphics.VBO
import arx.resource.ResourceManager
import org.lwjgl.opengl.GL11
import org.lwjgl.opengl.GL15
import overlock.atomicmap.AtomicMap

import scala.collection.mutable
import scala.language.postfixOps
import scalaxy.loops._

class TerrainGraphicsComponent(eng:GraphicsEngine) extends GraphicsComponent(eng) {
	lazy val shader = new WorldShader(world,new ForwardingModdable(pov _))
	lazy val textureBlock = new TextureBlock(1024,1024)

	val vbos = AtomicMap.atomicNBHM[TaleaCoord,AVBO]

	val pointHist = Metrics.histogram("terrain.points-per-talea")



	override def draw(): Unit = {
		GL.glSetState(GL11.GL_DEPTH_TEST, enable = true)

		shader.bind()
		pov.look()

		textureBlock.magFilter = GL11.GL_NEAREST
		textureBlock.minFilter = GL11.GL_NEAREST
		textureBlock.bind()

		for (vbo <- vbos.values) {
			vbo.solidifyIfNecessary(GL15.GL_STATIC_DRAW)
			vbo.drawElements()
		}
	}


	override protected def initialize(): Unit = {
		world[Terrain].material.allTaleae.map(t => TaleaCoord(t.position)).foreach(updateTaleaGraphics)
	}

	override protected def update(dt: UnitOfTime): Unit = {

	}

	def updateTaleaGraphics(tpos : TaleaCoord): Unit = {
		val vbo = vbos.getOrElseUpdate(tpos, new AVBO(WorldAttributeProfile))
		val terrain = world[Terrain]

		if (terrain.material.taleaAtRO(tpos).areAll(0.toShort)) {
			return
		}


		if (vbo.changeState(VBO.Dirty, VBO.Updating)) {
			vbo.clear()

			val pb = WorldAttributeProfile.createPointBuilder()
			val rpos = tpos.toObjectCoord
			val iter = TaleaIterator.withAdjacents(terrain.material, tpos)
			val lightIter = TaleaIterator.withAdjacents(world[Light].global(0), tpos)
			for (z <- 0 until Talea.dimension optimized; y <- 0 until Talea.dimension optimized; x <- 0 until Talea.dimension optimized) {
				iter.moveTo(x,y,z)
				lightIter.moveTo(x,y,z)

				val center = iter.center
				if (center != 0) {
					val mat = terrain.materialMapping(center)
					val tc = textureBlock(ResourceManager.image("eldr/entities/materials/textures/" + mat.name + ".png"))

					for (q <- 0 until 6 optimized) {
						if (iter.adj(q) == 0) {
							val vi = vbo.incrementVertexOffset(4)
							val ii = vbo.incrementIndexOffset(6)

							val lv = math.max(lightIter.adj(q) / 32.0f,0.1f)

							for (k <- 0 until 4 optimized) {
								val p = Cardinals.centeredCubePoints(q)(k)
								pb.setV(p.x + x.toFloat + rpos.x,p.y + y.toFloat + rpos.y,p.z + z.toFloat + rpos.z)
								pb.setTC(tc(k))
								pb.setGL(1.0f,1.0f,1.0f,lv)
								pb.setLL(0.0f,0.0f,0.0f,0.0f)
								vbo.setPoint(vi + k, pb)
							}
							vbo.setIQuad(ii,vi)
						}
					}
				}
			}

			pointHist.update(vbo.points.numElements)
			if (!vbo.changeState(VBO.Updating, VBO.Updated)) {
				Noto.error("VBO had invalid state upon update completion")
			}
		}
	}

}