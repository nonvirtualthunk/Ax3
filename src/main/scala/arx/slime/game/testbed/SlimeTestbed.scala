package arx.slime.game.testbed

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.core.datastructures.FiniteGrid2D
import arx.core.gen.ArxGenerators
import arx.core.gen.SimplexNoise
import arx.core.traits.TSentinel
import arx.core.units.UnitOfTime
import arx.core.vec._
import arx.engine.advanced.Engine
import arx.engine.data.TGameEntityAuxData
import arx.engine.data.TWorldAuxData
import arx.engine.entity.GameArchetype
import arx.engine.entity.GameEntity
import arx.engine.entity.TArchetypeKind
import arx.engine.entity.TGameEntity
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.CanvasGraphicsComponent
import arx.engine.graphics.components.GraphicsComponent
import arx.engine.graphics.data.PovData
import arx.engine.simple.Canvas
import arx.graphics.AVBO
import arx.graphics.TextureBlock
import arx.graphics.VBO
import arx.graphics.attributeprofiles.SimpleAttributeProfile
import arx.graphics.pov.TopDownCamera
import arx.resource.ResourceManager
import arx.slime.game.archetypes._
import arx.slime.game.archetypes.creatures.SimpleSlime
import arx.slime.game.core.HexCoord
import arx.slime.game.data.PhysicalData
import arx.slime.game.data.Terrain
//import arx.slime.game.testbed.control.SlimeControlMode
import arx.slime.graphics.components.EntityVisualizer
import arx.slime.graphics.components.TerrainVisualizer
import arx.slime.graphics.core.HexAttributeProfile
import org.lwjgl.opengl.GL11

import scalaxy.loops._

object SlimeTestbed extends Engine {


	override def setUpEngine(): Unit = {
		val camera = new TopDownCamera(10.0f)
		camera.proportionalMoveSpeed = true
		graphicsEngine.graphicsWorld[PovData].pov = camera

		graphicsEngine.addComponent[TerrainVisualizer]
		graphicsEngine.addComponent[EntityVisualizer]

//		controlEngine.pushMode(new SlimeControlMode(controlEngine))

		val TD = world[Terrain]

		import ArxGenerators._
		val gen = Scale(0.2f,0.2f,0.2f) >> Simplex(new SimplexNoise(154L))
		val subGen = Scale(0.1f,0.1f,0.1f) >> Fractal(3)(Simplex(new SimplexNoise(5543L)))

		val landRadius = TD.radius * 0.5f

		val rad = TD.radius * 2
		val center = HexCoord(0,0)
		for (q <- -TD.radius to TD.radius optimized ; r <- -TD.radius to TD.radius optimized) {
			val hx = HexCoord(q,r)
			val sz = TD.radius - 1
			if (absi(hx.q + hx.r) < sz && absi(hx.q) < sz && absi(hx.r) < sz) {
				val d = hx.toCartesian.lengthSafe

				val g = gen(hx.toCartesian)
				if (g < 1.0f - d / landRadius) {
					if (subGen(hx.toCartesian) < 0.5f) {
						TD(hx) = Grass
					} else if (subGen(hx.toCartesian) < 0.75f) {
						TD(hx) = Dirt
					} else {
						TD(hx) = Stone
					}
				} else {
					TD(hx) = Water
				}
			}
		}

		val relic = new GameEntity("Precursor Relic")
		relic.archetype = ThingArchetype.Relic
		relic[PhysicalData].position = HexCoord(0,0)

		world.addEntity(relic)

		val slime = SimpleSlime.createCreature(Nil)
		slime[PhysicalData].position = HexCoord(0,-10)

		world.addEntity(slime)
	}
}









