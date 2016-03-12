package arx.eldr.application.testbeds

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/28/16
  * Time: 1:46 PM
  */

import arx.Prelude._
import arx.core.datastructures.voxel.Talea
import arx.core.datastructures.voxel.TaleaIterator
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.metrics.Metrics
import arx.core.units.UnitOfTime
import arx.core.vec.Cardinals._
import arx.core.vec.coordinates.VoxelCoord
import arx.eldr.game.archetypes.Material
import arx.eldr.game.world.data.Light
import arx.eldr.game.world.data.Terrain
import arx.engine.EngineCore
import arx.engine.advanced.Engine
import arx.engine.game.GameEngine
import arx.engine.game.components.GameComponent
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.GraphicsComponent
import arx.engine.world.World
import arx.graphics._
import arx.graphics.attributeprofiles.SimpleAttributeProfile
import arx.graphics.pov.EyeCamera
import arx.resource.ResourceManager
import org.lwjgl.opengl.GL11
import scala.language.postfixOps
import scalaxy.loops._
import arx.core.vec._

object LightTestbed extends Engine {
	graphicsEngine.addComponent[BlockGraphics]
	gameEngine.addComponent[LightGameComponent]

	graphicsEngine.pov = {
		val cam = new EyeCamera(Vec3f(0, 0, 15), Vec3f.UnitX, Vec3f.UnitZ)
		cam.moveSpeed = Vec3f(0.35f)
		cam.turnSpeed = Vec2f(0.7f)
		cam
	}

	val terrain = world[Terrain]
	terrain.setMaterialsInRegion(VoxelRegion(VoxelCoord.Center - 20, VoxelCoord.Center + 20), VoxelCoord.transformToRelative((x, y, z) => {
		if (z == 0 || (x * x + y * y < 25 && z == 10)) {
			Material.withName("stone")
		} else {
			Material.Sentinel
		}
	}))

}

class LightGameComponent(eng: GameEngine, world: World) extends GameComponent(eng, world) {
	val index = 0

	override protected def initialize(): Unit = {
		import Cardinals._

		Metrics.timer("lighting initialization").timeStmt {
			val nodes = Metrics.timer("initial flow").timeStmt {
				flowInitialLight()
			}

			Metrics.timer("cascade").timeStmt {
				cascadeInitialLight(nodes)
			}
		}

		Metrics.prettyPrint()
	}

	def cascadeInitialLight(nodes: FastLightNodeQueue): Unit = {
		val light = world[Light].global(index)
		val terrain = world[Terrain].material

		while (nodes.nonEmpty) {
			nodes.dequeue()

			val x = nodes.x
			val y = nodes.y
			val z = nodes.z
			val l = nodes.lightValue
			val cf = nodes.cameFrom

			val curL = light(x,y,z)
			if (curL < l && l > 0) {
				light(x,y,z) = l.toByte
				for (q <- 0 until 6 optimized) {
					if (q != oppositeDirection(cf)) {
						val ax = x + cardinalsX(q)
						val ay = y + cardinalsY(q)
						val az = z + cardinalsZ(q)
						if (light(ax,ay,az) < l-1 && terrain(ax,ay,az) == 0) {
							nodes.enqueue(ax, ay, az, q, l-1)
						}
					}
				}
			}
		}
	}

	def flowInitialLight() : FastLightNodeQueue = {
		val edges = new FastLightNodeQueue

		val light = world[Light].global(index)
		val terrain = world[Terrain].material
		val maxLight = world[Light].MaxLight

		val squareAdjX = Array(-1, 1, 0, 0)
		val squareAdjY = Array(0, 0, -1, 1)
		val qMap = Array(Cardinals.Right,Cardinals.Left,Cardinals.Forward,Cardinals.Back) //mapping from square adj q to regular q, opposite

		val fullRegion = VoxelRegion(terrain.grid.values.map(t => t.position))
		val maxZ = fullRegion.max.z
		val minZ = fullRegion.min.z

		var set = Set[ReadVec2i]()
		for (sv <- ((fullRegion.min.xy >> Talea.dimensionPo2) - 1) to ((fullRegion.max.xy >> Talea.dimensionPo2) + 1)) {
			val obstructed = Array.fill(Talea.dimension + 2, Talea.dimension + 2)(false)
			val allObstructed = Array.fill(Talea.dimension, Talea.dimension)(false)
			var obstructionCount = 0

			for (sz <- (maxZ >> Talea.dimensionPo2) to (minZ >> Talea.dimensionPo2) by -1 optimized) {
				val v = sv << Talea.dimensionPo2
				val vz = sz << Talea.dimensionPo2

				val tt = terrain.taleaAtRO(v.x, v.y, vz)
				val tIter = TaleaIterator(terrain,VoxelCoord(v.x,v.y,vz))
				// if they're all 0, and we haven't obstructed anything, we can skip
				if (!tt.areAll(0.toShort) || obstructionCount > 0) {
					val lt: Talea[Byte] = light.taleaAt(v.x, v.y, vz)
					for (z <- Talea.dimension-1 to 0 by -1 optimized) {
						// pick up all the edges
						// skip if obstr count == all?
						for (b <- Array(-1,Talea.dimension)) {
							for (a <- -1 to Talea.dimension optimized) {
								if (!obstructed(a+1)(b+1)) {
									obstructed(a+1)(b+1) = terrain(v.x+a,v.y+b,vz+z) != 0
								}
								if (!obstructed(b+1)(a+1)) {
									obstructed(b+1)(a+1) = terrain(v.x+b,v.y+a,vz+z) != 0
								}
							}
						}

						for (y <- 0 until Talea.dimension optimized; x <- 0 until Talea.dimension optimized) {
							tIter.moveTo(x,y,z)
							if (obstructed(x + 1)(y + 1)) {
								if (!allObstructed(x)(y) && tIter.value == 0) {
									// light is 0! I guess add this coordinate to a queue or something
									var foundQ = -1
									var q = 0
									while (q < 4 && foundQ == -1) {
										if (!obstructed(x + 1 + squareAdjX(q))(y + 1 + squareAdjY(q))) {
											foundQ = q
										}
										q += 1
									}

									if (foundQ == -1) {
										allObstructed(x)(y) = true
									} else {
										edges.enqueue(v.x+x,v.y+y,vz+z,qMap(foundQ),maxLight-1)
									}
								}

								lt(x,y,z) = 0
							} else {
								if (tIter.value != 0) {
									obstructed(x + 1)(y + 1) = true
									obstructionCount += 1

									lt(x,y,z) = 0
								} else {
									// do nothing, max light is fine
								}
							}
						}
					}
				}
			}
		}

		edges
	}

	override protected def update(dt: UnitOfTime): Unit = {

	}
}

class FastLightNodeQueue {
	val power = 17
	var capAND = (1 << power) - 1
	var capacity = (1 << power)
	var front = 0
	var lastFront = 0
	var back = 0
	var backingArray = Array.ofDim[Int](capacity)

	def nonEmpty = front != back
	def enqueue( x : Int , y : Int , z : Int , cameFrom : Int , lightValue : Int) {
		backingArray(back+0) = x
		backingArray(back+1) = y
		backingArray(back+2) = z
		backingArray(back+3) = cameFrom | (lightValue << 10)
		back = (back + 4) & capAND
	}
	def dequeue() { lastFront = front; front = (front + 4) & capAND }

	def x = backingArray(lastFront)
	def y = backingArray(lastFront+1)
	def z = backingArray(lastFront+2)
	def cameFrom = (backingArray(lastFront+3) & 0x000000ff)
	def lightValue = (backingArray(lastFront+3) >> 10)

	def clear () { front = back }
}

class BlockGraphics(eng: GraphicsEngine, world: World) extends GraphicsComponent(eng, world) {
	lazy val shader = ResourceManager.shader("shaders/Simple")

	lazy val textureBlock = new TextureBlock(1024, 1024)

	val vbo = new AVBO(SimpleAttributeProfile)

	override def draw(): Unit = {
		GL.glSetState(GL11.GL_DEPTH_TEST, true)

		shader.bind()
		pov.look()

		textureBlock.magFilter = GL11.GL_NEAREST
		textureBlock.minFilter = GL11.GL_NEAREST
		textureBlock.bind()

		vbo.solidifyIfNecessary()
		vbo.drawElements()
	}

	override protected def update(dt: UnitOfTime): Unit = {
		if (vbo.changeState(VBO.Dirty, VBO.Updating)) {
			vbo.useIntIndices()
			val region = VoxelRegion(VoxelCoord.Center - 20, VoxelCoord.Center + 20)
			val terrain = world[Terrain]

			val pb = SimpleAttributeProfile.createPointBuilder()
			for (shifted <- (region.min >> Talea.dimensionPo2) to (region.max >> Talea.dimensionPo2)) {
				val tpos = VoxelCoord(shifted << Talea.dimensionPo2)
				val rpos = tpos.toObjectCoord
				val iter = TaleaIterator.withAdjacents(terrain.material, tpos)
				val lightIter = TaleaIterator.withAdjacents(world[Light].global(0), tpos)
				for (z <- 0 until Talea.dimension optimized; y <- 0 until Talea.dimension optimized; x <- 0 until Talea.dimension optimized) {
					iter.moveTo(x, y, z)
					lightIter.moveTo(x, y, z)

					val center = iter.center
					if (center != 0) {
						val mat = terrain.materialMapping(center)
						val tc = textureBlock(ResourceManager.image("eldr/entities/materials/textures/" + mat.name + ".png"))

						for (q <- 0 until 6 optimized) {
							if (iter.adj(q) == 0) {
								val vi = vbo.incrementVertexOffset(4)
								val ii = vbo.incrementIndexOffset(6)

								val lv = math.max(lightIter.adj(q) / 32.0f, 0.1f)

								for (k <- 0 until 4 optimized) {
									val p = Cardinals.centeredCubePoints(q)(k)
									pb.setV(p.x + x.toFloat + rpos.x, p.y + y.toFloat + rpos.y, p.z + z.toFloat + rpos.z)
									pb.setTC(tc(k))
									pb.setC(lv, lv, lv, 1.0f)
									vbo.setPoint(vi + k, pb)
								}
								vbo.setIQuad(ii, vi)
							}
						}
					}
				}
			}



			//			vbo.incrementVertexOffset(4)
			//			vbo.incrementIndexOffset(6)
			//
			//			val depth = 0.0f
			//			val size = 10
			//
			//			val pb = SimpleAttributeProfile.createPointBuilder()
			//			pb.setV(-size,-size,depth)
			//			pb.setTC(0.0f,0.0f)
			//			pb.setC(1.0f,1.0f,1.0f,1.0f)
			//			vbo.setPoint(0,pb)
			//
			//			pb.setV(size,-size,depth)
			//			pb.setTC(1.0f,0.0f)
			//			vbo.setPoint(1,pb)
			//
			//			pb.setV(size,size,depth)
			//			pb.setTC(1.0f,1.0f)
			//			vbo.setPoint(2,pb)
			//
			//			pb.setV(-size,size,depth)
			//			pb.setTC(0.0f,1.0f)
			//			vbo.setPoint(3,pb)
			//
			//			vbo.setIQuad(0,0)
			vbo.state.set(VBO.Updated)
		}
	}
}