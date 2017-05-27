package arx.eldr.application.testbeds

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/28/16
  * Time: 1:46 PM
  */

import arx.Prelude._
import arx.core.Moddable
import arx.core.datastructures.voxel.Talea
import arx.core.datastructures.voxel.TaleaIterator
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.VoxelCoord
import arx.eldr.game.archetypes.Material
import arx.eldr.game.logic.light.LightComponent
import arx.eldr.game.logic.light.StandardGlobalLightComputor
import arx.eldr.game.world.data.Light
import arx.eldr.game.world.data.Terrain
import arx.eldr.graphics.environment.SkyboxGraphicsComponent
import arx.eldr.graphics.environment.TerrainGraphicsComponent
import arx.eldr.graphics.environment.WorldAttributeProfile
import arx.eldr.graphics.environment.WorldShader
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
import org.lwjgl.opengl.GL12
import org.lwjgl.opengl.GL15

import scala.language.postfixOps
import scalaxy.loops._
import arx.core.vec._
import arx.eldr.graphics.entities.EntityGraphicsComponent
import arx.engine.entity.GameEntity
import arx.engine.graphics.data.PovData

object LightTestbed extends Engine {

	override def setUpEngine(): Unit = {

		//	graphicsEngine.addComponent[BlockGraphics]
		graphicsEngine.addComponent[TerrainGraphicsComponent]
		graphicsEngine.addComponent[SkyboxGraphicsComponent]
		//	gameEngine.addComponent[ExperimentalLightGameComponent]
		gameEngine.addComponent[LightComponent]

		graphicsEngine.graphicsWorld[PovData].pov = {
			val cam = new EyeCamera(Vec3f(0,0,15),Vec3f.UnitX,Vec3f.UnitZ)
			cam.moveSpeed = Vec3f(0.35f)
			cam.turnSpeed = Vec2f(0.7f)
			cam.viewDistance = 200.0f
			cam
		}

		val terrain = world[Terrain]
		terrain.setMaterialsInRegion(VoxelRegion(VoxelCoord.Center-20,VoxelCoord.Center+20), VoxelCoord.transformToRelative((x,y,z) => {
			if (z == 0 || (x*x+y*y < 25 && z == 10)) {
				Material.withName("stone")
			} else if (x == -20 && z <= 0) {
				Material.withName("stone")
			} else {
				Material.Sentinel
			}
		}))
	}

}

class ExperimentalLightGameComponent(eng:GameEngine) extends GameComponent(eng) {
	override protected def initialize(): Unit = {
		val index = 0

		val light = world[Light].global(index)
		val terrain = world[Terrain].material

		val fullRegion = VoxelRegion(terrain.grid.values.map(t => t.position))
		val maxZ = fullRegion.max.z
		val minZ = fullRegion.min.z
		for (sv <- ((fullRegion.min.xy >> Talea.dimensionPo2)-1) to ((fullRegion.max.xy >> Talea.dimensionPo2)+1)) {
			val obstructed = Array.fill(Talea.dimension,Talea.dimension)(false)
			var obstructionCount = 0

			for (sz <- (maxZ >> Talea.dimensionPo2) to (minZ >> Talea.dimensionPo2) by -1 optimized) {
				val v = sv << Talea.dimensionPo2

				val tt = terrain.taleaAtRO(v.x,v.y,sz << Talea.dimensionPo2)
				// if they're all 0, and we haven't obstructed anything, we can skip
				if (! tt.areAll(0.toShort) || obstructionCount > 0) {
					var lt : Talea[Byte] = null
					for (z <- Talea.dimension-1 to 0 by -1 optimized; y <- 0 until Talea.dimension optimized; x <- 0 until Talea.dimension optimized) {
						if (obstructed(x)(y)) {
							// light is 0! I guess add this coordinate to a queue or something
						} else {
							if (tt(x,y,z) != 0) {
								obstructed(x)(y) = true
								obstructionCount += 1
							} else {
								if (lt == null) {
									lt = light.taleaAt(v.x,v.y,sz << Talea.dimensionPo2)
								}
								lt(x,y,z) = 32
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

class BlockGraphics(eng:GraphicsEngine) extends GraphicsComponent(eng) {
//	lazy val shader = ResourceManager.shader("shaders/Simple")
	lazy val shader = new WorldShader(world, pov)

	lazy val textureBlock = new TextureBlock(1024,1024)

	val vbo = new AVBO(WorldAttributeProfile)

	override def draw(): Unit = {
		GL.glSetState(GL11.GL_DEPTH_TEST, true)

		shader.bind()
		pov.look()

		textureBlock.magFilter = GL11.GL_NEAREST
		textureBlock.minFilter = GL11.GL_NEAREST
		textureBlock.bind()

		vbo.solidifyIfNecessary(GL15.GL_STATIC_DRAW)
		vbo.drawElements()
	}

	override protected def update(dt: UnitOfTime): Unit = {
		if (vbo.changeState(VBO.Dirty, VBO.Updating)) {
			val region = VoxelRegion(VoxelCoord.Center-20,VoxelCoord.Center+20)
			val terrain = world[Terrain]

			val pb = WorldAttributeProfile.createPointBuilder()
			for (shifted <- (region.min >> Talea.dimensionPo2) to (region.max >> Talea.dimensionPo2)) {
				val tpos = VoxelCoord(shifted << Talea.dimensionPo2)
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
//									pb.setC(lv,lv,lv,1.0f)
									pb.setGL(1.0f,1.0f,1.0f,lv)
									pb.setLL(0.0f,0.0f,0.0f,0.0f)
									vbo.setPoint(vi + k, pb)
								}
								vbo.setIQuad(ii,vi)
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