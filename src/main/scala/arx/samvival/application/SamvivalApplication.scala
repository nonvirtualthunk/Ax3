package arx.samvival.application

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/18/18
  * Time: 8:32 AM
  */

import arx.Prelude._
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import scalaxy.loops._
import arx.core.vec._
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.EngineCore
import arx.engine.advanced.{Engine, LEngine}
import arx.engine.control.components.windowing.WindowingControlComponent
import arx.engine.data.TimeData
import arx.engine.entity.GameEntity
import arx.engine.graphics.components.windowing.WindowingGraphicsComponent
import arx.engine.graphics.data.PovData
import arx.graphics.helpers.{Color, HSBA}
import arx.graphics.pov.TopDownCamera
import arx.samvival.graphics.SamvivalTestGraphicsComponent

object SamvivalApplication extends LEngine {
	EngineCore.windowWidth = 1000
	EngineCore.windowHeight = 1000

	override def setUpEngine(): Unit = {
		graphicsEngine.parallelism = 1
		gameEngine.parallelism = 1
		serialGameEngine = true
		serialGraphicsEngine = true

		val pov = new TopDownCamera(20)
		pov.fovy = 70.0f
		graphicsWorld[PovData].pov = pov

//		gameEngine.addComponent[RogMainGameComponent]
//		gameEngine.addComponent[RogPhysicsGameComponent]
//		gameEngine.addComponent[RogLightingGameComponent]
//		gameEngine.addComponent[RogVisionGameComponent]
//		gameEngine.addComponent[RogLogbookGameComponent]
//
//		graphicsEngine.addComponent[TerrainGraphicsComponent]
//		graphicsEngine.addComponent[EntityGraphicsComponent]
//		graphicsEngine.addComponent[WindowingGraphicsComponent]
//		graphicsEngine.addComponent[OverlayGraphicsComponent]

		graphicsEngine.addComponent[SamvivalTestGraphicsComponent]

//		controlEngine.addComponent[RogCharacterControl]
//		controlEngine.addComponent[WindowingControlComponent]

//		gameEngine.eventBus.onEvent {
//			case AdvanceWorldEvent(dt) =>
//				synchronized {
//					gameEngine.updateSerial(dt.inSeconds)
//					world[TimeData].time += dt
//				}
//		}
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
