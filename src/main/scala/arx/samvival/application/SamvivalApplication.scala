package arx.samvival.application

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/18/18
  * Time: 8:32 AM
  */

import arx.Prelude._
import arx.engine.EngineCore
import arx.engine.advanced.LEngine
import arx.engine.control.components.windowing.{LWindowingControlComponent, WindowingControlComponent}
import arx.engine.graphics.components.windowing.{LWindowingGraphicsComponent, WindowingGraphicsComponent}
import arx.engine.graphics.data.PovData
import arx.engine.lworld.LWorld
import arx.graphics.pov.PixelCamera
import arx.samvival.control.MainMapControlModeComponent
import arx.samvival.game.entities.SVAuxData
import arx.samvival.game.logic.{Scenario, TerrainGenerator}
import arx.samvival.graphics.animation.{AnimationGraphicsComponent, TemporalAnimationGraphicsComponent}
import arx.samvival.graphics.components._

object SamvivalApplication extends LEngine {
	EngineCore.windowWidth = 1440
	EngineCore.windowHeight = 900


	override def registerTypes(world: LWorld): Unit = {
		world.registerSubtypesOf[SVAuxData]()
	}

	override def setUpEngine(): Unit = {
		graphicsEngine.parallelism = 1
		gameEngine.parallelism = 1
		serialGameEngine = true
		serialGraphicsEngine = true

		graphicsWorld[PovData].pov = new PixelCamera(512, 0.1f)

		TerrainGenerator.generate(world)

//		gameEngine.addComponent[RogMainGameComponent]
//		gameEngine.addComponent[RogPhysicsGameComponent]
//		gameEngine.addComponent[RogLightingGameComponent]
//		gameEngine.addComponent[RogVisionGameComponent]
//		gameEngine.addComponent[RogLogbookGameComponent]
//
//		graphicsEngine.addComponent[TerrainGraphicsComponent]
//		graphicsEngine.addComponent[EntityGraphicsComponent]
		graphicsEngine.addComponent[LWindowingGraphicsComponent]
//		graphicsEngine.addComponent[OverlayGraphicsComponent]

//		graphicsEngine.addComponent[SamvivalTestGraphicsComponent]
		graphicsEngine.addComponent[TileGraphicsComponent]
		graphicsEngine.addComponent[CullingGraphicsComponent]
		graphicsEngine.addComponent[TemporalAnimationGraphicsComponent]
		graphicsEngine.addComponent[AnimationGraphicsComponent]
		graphicsEngine.addComponent[CharacterGraphicsComponent]
		graphicsEngine.addComponent[OverlayGraphicsComponent]

		controlEngine.addComponent[MainMapControlModeComponent]


//		controlEngine.addComponent[RogCharacterControl]
		controlEngine.addComponent[LWindowingControlComponent]

		Scenario.loadTestScenario(world)
	}


	override def init(): Unit = {
		super.init()
		controlEngine.components.firstOfType[MainMapControlModeComponent].get.pushMode[MainMapControlModeComponent]
	}

	override def update(deltaSeconds: Float): Unit = {
		controlEngine.update(deltaSeconds)
	}


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
	}

}
