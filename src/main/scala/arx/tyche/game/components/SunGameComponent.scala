package arx.tyche.game.components

import arx.core.units.UnitOfTime
import arx.engine.game.GameEngine
import arx.engine.game.components.GameComponent
import arx.tyche.game.data.Sun
import org.lwjgl.glfw.GLFW

class SunGameComponent(ge : GameEngine) extends GameComponent(ge) {

	var sunMotionPerSecond = Math.PI.toFloat * 0.005f

	var lastUpdate = -1.0
	override protected def updateSelf(ignored: UnitOfTime): Unit = {
		if (lastUpdate < 0.0) { lastUpdate = GLFW.glfwGetTime() }
		val updatedAt = GLFW.glfwGetTime()
		val deltaT = updatedAt - lastUpdate

		world[Sun].theta += sunMotionPerSecond * deltaT.toFloat

		lastUpdate = updatedAt
	}
}
