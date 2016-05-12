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
import arx.core.datastructures.voxel.VoxelGrid
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
		val cam = new EyeCamera(Vec3f(-20, 0, 25), Vec3f(1.0f,0.0f,-0.5f).normalize, Vec3f.UnitZ)
		cam.moveSpeed = Vec3f(0.35f)
		cam.turnSpeed = Vec2f(0.7f)
		cam
	}

	val terrain = world[Terrain]
	terrain.setMaterialsInRegion(VoxelRegion(VoxelCoord.Center - 20, VoxelCoord.Center + 20), VoxelCoord.transformToRelative((x, y, z) => {
		val sq = x * x + y * y
		if ((z == 0 && sq < 900) || (sq < 64 && z == 10)) {
			Material.withName("stone")
		} else {
			Material.Sentinel
		}
	}))

	VoxelRegion(VoxelCoord.fromRelative(15,15,0), VoxelCoord.fromRelative(17,17,15)).foreachUnsafe(v => terrain.material(v) = 1.toShort)

}

class LightGameComponent(eng: GameEngine, world: World) extends GameComponent(eng, world) {
	val index = 0

	override protected def initialize(): Unit = {
		import Cardinals._

		Metrics.timer("lighting initialization").timeStmt {
			val nodes = Metrics.timer("initial flow").timeStmt {
				flowInitialLight()
			}
		}

		Metrics.prettyPrint()
	}

	def opacityOf(s: Short) = {
		if (s == 0) {
			0
		} else {
			100
		}
	}

	// so we could make a stepping down screen, but it will require an array of bytes the size of the entire world in
	// two dimensions
	def flowInitialLight(): FastLightNodeQueue = {
		val light = world[Light].global(index)
		val terrain = world[Terrain].material

		val maxLight = world[Light].MaxLight.toByte

		val fullRegion = VoxelRegion(terrain.grid.values.map(t => t.position))
		val sftMin = fullRegion.min.downShifted
		val sftMax = fullRegion.max.downShifted

		val effDim = (sftMax - sftMin + 1).upShifted
		val state = new LightScreen(sftMin.upShifted.xy, effDim.xy, maxLight)
		val tvalues = Array.ofDim[Short](Talea.dimension)
		val lvalues = Array.ofDim[Byte](Talea.dimension)
		val edges = new FastLightNodeQueue

		// the sftMax is the origin of the topmost talea, we want to start from the top of that talea instead, hence +1
		for (absZ <- (sftMax + 1).upShifted.z to sftMin.upShifted.z by -1 optimized) {
			if (absZ % 3 == 0) {
//				state.shift()
			}

			for (sy <- sftMin.y to sftMax.y optimized; sx <- sftMin.x to sftMax.x optimized) {
				val absX = sx << Talea.dimensionPo2
				val absY = sy << Talea.dimensionPo2

				val tt = terrain.taleaAtRO(absX, absY, absZ)
				val lt = light.taleaAt(absX, absY, absZ)
				for (y <- 0 until Talea.dimension optimized) {
					tt.loadRow(0, y, absZ - tt.z, Talea.dimension, 0, tvalues)
					var allFull = true
					for (x <- 0 until Talea.dimension optimized) {
						val opacity = opacityOf(tvalues(x))
						val curValue = state(absX + x, absY + y)
						if (opacity == 100) {
							// obstructed
							if (curValue == -1) {
								// obstruction continues, no action required
							} else {
								// just hit an obstruction, switch state to -1
								state(absX + x, absY + y) = -1
							}
							allFull = false
						} else {
							// non-obstructed
							if (curValue == -1) {
								// just cleared an obstruction, need to find out what our best adjacent light will be
								edges.enqueue(absX + x, absY + y, absZ, 10, 0)
								allFull = false
							} else {
								if (curValue == maxLight) {
									// do nothing, no obstacle, max light, everything's happy
								} else {
									// we're at less than max light, therefore we are attenuating
//									state(absX + x, absY + y) = math.max(curValue - 1, 0).toByte
									allFull = false
								}
							}
						}
					}

					if (!allFull) {
						val row = state.rowAtY(absY + y)
						val offset = state.offsetForX(absX)
						lt.storeRow(y,absZ - lt.z,row, offset, lvalues)
					}
				}
			}

			flowOutEdges(light, terrain, state, absZ, edges)
		}

		edges
	}

	//	def flowInitialLight() : FastLightNodeQueue = {
	//		val edges = new FastLightNodeQueue
	//
	//		val light = world[Light].global(index)
	//		val terrain = world[Terrain].material
	//
	//		val fullRegion = VoxelRegion(terrain.grid.values.map(t => t.position))
	//		val maxZ = fullRegion.max.z
	//		val minZ = fullRegion.min.z
	//
	//		for (sv <- (fullRegion.min.downShifted.xy - 1) to (fullRegion.max.downShifted.xy + 1)) {
	//			val obstructed = Array.fill(Talea.dimension, Talea.dimension)(false)
	//			var obstructionCount = 0
	//
	//			for (sz <- (maxZ >> Talea.dimensionPo2) to (minZ >> Talea.dimensionPo2) by -1 optimized) {
	//				val v = sv << Talea.dimensionPo2
	//				val vz = sz << Talea.dimensionPo2
	//
	//				val tt = terrain.taleaAtRO(v.x, v.y, vz)
	//
	//				// if they're all 0, and we haven't obstructed anything, we can skip
	//				if (!tt.areAll(0.toShort) || obstructionCount > 0) {
	//					val lt: Talea[Byte] = light.taleaAt(v.x, v.y, vz)
	//					for (z <- Talea.dimension-1 to 0 by -1 optimized) {
	//						for (y <- 0 until Talea.dimension optimized; x <- 0 until Talea.dimension optimized) {
	//							if (obstructed(y)(x)) {
	//								lt(x,y,z) = 0
	//							} else {
	//								if (tt(x,y,z) != 0) {
	//									obstructed(y)(x) = true
	//									obstructionCount += 1
	//
	//									lt(x,y,z) = 0
	//								} else {
	//									// do nothing, max light is fine
	//								}
	//							}
	//						}
	//					}
	//				}
	//			}
	//		}
	//
	//		edges
	//	}
	//              0  1  2  3  4  5  6  7  8  9
	val cX = Array(-1,-1,-1, 0, 1, 1, 1, 0, 0, 0)
	val cY = Array(-1, 0, 1, 1, 1, 0,-1,-1, 0, 0)
	val cZ = Array( 0, 0, 0, 0, 0, 0, 0, 0, 1,-1)
	val dL = Array( 7, 5, 7, 5, 7, 5, 7, 5, 5, 5)
	val oD = Array( 4, 5, 6, 7, 0, 1, 2, 3, 9, 8, 10)

	def flowOutEdges(light: VoxelGrid[Byte], terrain: VoxelGrid[Short], state: LightScreen, absZ: Int, edges: FastLightNodeQueue): Unit = {
		val secondaryEdges = new FastLightNodeQueue

		while (edges.nonEmpty) {
			edges.dequeue()
			val x = edges.x
			val y = edges.y
			val z = edges.z
			val cf = edges.cameFrom
			val l = if (cf == 10) {
				var bestL = 0
				for (q <- 0 until 8 optimized) {
					val ax = x + cX(q)
					val ay = y + cY(q)
					val az = z + cZ(q)

					val al = light(ax, ay, az)
					bestL = math.max(al - dL(q), bestL)
				}
				math.max(0, bestL)
			} else {
				edges.lightValue
			}

			val cur = light(x, y, z)
			if (cur < l) {
				secondaryEdges.enqueue(x,y,z,8,l) // this is for dealing with the s-pipe situation, we're going to need
				// another loop through afterwards, and probably a piece of information in the state grid that indicates
				// whether or not the column in question has become disconnected from the lightflow
				light(x, y, z) = l.toByte
				if (z == absZ) {
					state(x, y) = l.toByte
				}

				for (q <- 0 until 8 optimized) {
					if (q != oD(cf)) {
						val az = z + cZ(q)
						if (az >= absZ) {
							val ax = x + cX(q)
							val ay = y + cY(q)


							val op = opacityOf(terrain(ax, ay, az))
							if (op != 100) {
								val al = light(ax, ay, az)
								if (al < l - dL(q)) {
									edges.enqueue(ax, ay, az, q, l - dL(q))
								}
							}
						}
					}
				}
			}
		}
	}
	override protected def update(dt: UnitOfTime): Unit = {

	}
}

class LightScreen(val min: ReadVec2i, val dim: ReadVec2i, fillWith: Byte) {
	protected val backing = Array.fill(dim.y, dim.x)(fillWith)

	private[this] final val minY = min.y
	private[this] final val minX = min.x

	def apply(x: Int, y: Int) = backing(y - minY)(x - minX)
	def update(x: Int, y: Int, b: Byte) = backing(y - minY)(x - minX) = b

	def rowAtY(y: Int) = backing(y - minY)
	def offsetForX(x: Int) = x - minX
	
	def shift(): Unit = {
		for (y <- 0 until dim.y-1) {
			backing(y) = backing(y + 1)
		}
		backing(dim.y - 1) = Array.fill(dim.x)(fillWith)
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
	def enqueue(x: Int, y: Int, z: Int, cameFrom: Int, lightValue: Int) {
		backingArray(back + 0) = x
		backingArray(back + 1) = y
		backingArray(back + 2) = z
		backingArray(back + 3) = cameFrom | (lightValue << 10)
		back = (back + 4) & capAND
	}
	def dequeue() { lastFront = front; front = (front + 4) & capAND }

	def x = backingArray(lastFront)
	def y = backingArray(lastFront + 1)
	def z = backingArray(lastFront + 2)
	def cameFrom = (backingArray(lastFront + 3) & 0x000000ff)
	def lightValue = (backingArray(lastFront + 3) >> 10)

	def clear() { front = back }
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
						val tc = textureBlock(ResourceManager.image("eldr/entities/materials/textures/" + /*mat.name*/"bordered" + ".png"))

						for (q <- 0 until 6 optimized) {
							if (iter.adj(q) == 0) {
								val vi = vbo.incrementVertexOffset(4)
								val ii = vbo.incrementIndexOffset(6)

								val lv = math.max(lightIter.adj(q) / 127.0f, 0.1f)

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