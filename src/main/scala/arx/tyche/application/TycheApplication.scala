package arx.tyche.application

import arx.engine.EngineCore
import arx.engine.advanced.Engine
import arx.engine.graphics.components.windowing.LWindowingGraphicsComponent
import arx.engine.graphics.data.PovData
import arx.engine.lworld.LWorld
import arx.graphics.pov.{FixedCamera, PixelCamera}
import arx.Prelude._
import arx.core.vec.Vec3f
import arx.engine.entity.GameEntity
import arx.engine.world.World
import arx.graphics.GL
import arx.tyche.control.components.MainControlComponent
import arx.tyche.core.GlobeCoord
import arx.tyche.core.data.TyAuxData
import arx.tyche.game.components.{SpiritGameComponent, SunGameComponent, TerrainGameComponent, TransitionGameComponent}
import arx.tyche.game.data.SourceType.Mackerel
import arx.tyche.game.data.TerrainType.Ocean
import arx.tyche.game.data.{Globe, Patch, PatchShape, Spirit, TerrainType, TransformTerrain}
import arx.tyche.game.logic.GlobeGenerator
import arx.tyche.graphics.components.{GlobeGraphics, ParticleGraphics, PatchSourceGraphics, SpiritGraphics, SunGraphics}
import arx.tyche.graphics.core.GlobeCamera

object TycheApplication extends Engine {
	EngineCore.windowWidth = 1440
	EngineCore.windowHeight = 900


	override def registerTypes(world: World): Unit = {
//		world.registerSubtypesOf[TyAuxData]()
	}

	override def setUpEngine(): Unit = {
		graphicsEngine.parallelism = 1
		gameEngine.parallelism = 1
		serialGameEngine = true
		serialGraphicsEngine = true

//		graphicsWorld[PovData].pov = new FixedCamera(Vec3f(50.0f,0.0f,0.0f), Vec3f(-1.0f,0.0f,0.0f), Vec3f(0.0f,0.0f,1.0f))
		graphicsWorld[PovData].pov = new GlobeCamera

		GlobeGenerator.generate(world, 1.0f)

		val oceanSpirit = Spirit.oceanSpirit(world)

		world[Globe].patches.head.aux[Patch].terrainType = Ocean
		world[Globe].patches.head.aux[Patch].sourceType = Mackerel

		gameEngine.addComponent[SunGameComponent]
		gameEngine.addComponent[SpiritGameComponent]
		gameEngine.addComponent[TransitionGameComponent]
		gameEngine.addComponent[TerrainGameComponent]

		graphicsEngine.addComponent[GlobeGraphics]
		graphicsEngine.addComponent[SunGraphics]
		graphicsEngine.addComponent[SpiritGraphics]
		graphicsEngine.addComponent[ParticleGraphics]
		graphicsEngine.addComponent[PatchSourceGraphics]

		controlEngine.addComponent[MainControlComponent]
		//		gameEngine.addComponent[RogMainGameComponent]
	}


	override def init(): Unit = {
		super.init()
//		controlEngine.components.firstOfType[MainMapControlModeComponent].get.pushMode[MainMapControlModeComponent]
	}

//	override def update(deltaSeconds: Float): Unit = {
//		controlEngine.update(deltaSeconds)
//		gameEngine.update(deltaSeconds)
//	}


	var lastDraw = -1.seconds

	override def draw(): Unit = {
		val now = curTime()
		val delta = if (lastDraw < 0.seconds) {
			0.01666666.seconds
		} else {
			now - lastDraw
		}
		graphicsEngine.updateSerial(delta.inSeconds)
		lastDraw = now
		super.draw()

		GL.checkError()
	}

}
