package arx.rog2.control.modes

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 6/4/17
  * Time: 6:00 PM
  */

import arx.Prelude._

import scalaxy.loops._
import arx.core.vec._
import arx.engine.control.event.Event.TEventUser
import arx.rog2.control.RogCharacterControl
import org.lwjgl.glfw.GLFW._

trait ControlMode extends TEventUser {
	var characterControl : RogCharacterControl = _

	def initialize()

	def close()

	def isTopLevel = characterControl.modeStack.head == this


	def keyToVector(key : Int) = key match {
		case GLFW_KEY_W => Vec3i(0, 1, 0)
		case GLFW_KEY_S => Vec3i(0, -1, 0)
		case GLFW_KEY_A => Vec3i(-1, 0, 0)
		case GLFW_KEY_D => Vec3i(1, 0, 0)
		case GLFW_KEY_Q => Vec3i(-1, 1, 0)
		case GLFW_KEY_E => Vec3i(1, 1, 0)
		case GLFW_KEY_C => Vec3i(1, -1, 0)
		case GLFW_KEY_Z => Vec3i(-1, -1, 0)
		case _ => Vec3i.Zero
	}
}
