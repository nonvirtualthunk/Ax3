package arx.tyche.graphics.core

import arx.Prelude.float2RicherFloat
import arx.core.mat.ReadMat4x4
import arx.core.math.Recti
import arx.core.units.UnitOfTime
import arx.core.vec.{ReadVec3f, Vec3f}
import arx.engine.control.event.Event.{KeyPressEvent, Keymap}
import arx.graphics.GL
import arx.graphics.pov.TCamera
import arx.tyche.core.GlobeCoord
import arx.tyche.graphics.core.GlobeCamera._
import org.joml.{Quaternionf, Vector3f}
import org.lwjgl.glfw.GLFW

class GlobeCamera extends TCamera {
	var rotation = new Quaternionf()
	var minZ = 1.0f
	var maxZ = 4.0f
	var z = 2.0f
	fovy = 70.0f

	var forwardQuat = GlobeCoord.quaternionFromEuler(1f, 0.0f)
	var upQuat = GlobeCoord.quaternionFromEuler(0.0f, 1f)


	override def modelviewMatrix(viewport: Recti): ReadMat4x4 = {
		val up = rotation.transform(new Vector3f(0.0f,0.0f,1.0f))
		GL.lookAt(eye, Vec3f.Zero, Vec3f(up.x, up.y, up.z))
	}

	override def projectionMatrix(viewport: Recti): ReadMat4x4 = {
		GL.perspective(fovy,viewport.w/viewport.h.toFloat,near,far)
	}

	override def eye: ReadVec3f = {
		val tmp = rotation.transform(new Vector3f(z,0.0f,0.0f))
		Vec3f(tmp.x, tmp.y, tmp.z)
	}

	override def forward: ReadVec3f = {
		(Vec3f.Zero - eye).normalizeSafe
	}

	override def ortho: ReadVec3f = {
		val tmp = rotation.transform(new Vector3f(0.0f,1.0f,0.0f))
		Vec3f(tmp.x, tmp.y, tmp.z)
	}

	override def up: ReadVec3f = {
		val tmp = rotation.transform(new Vector3f(0.0f,0.0f,1.0f))
		Vec3f(tmp.x, tmp.y, tmp.z)
	}

	var lastUpdate = -1.0
	override def update(dt: UnitOfTime): Unit = {

	}


	override def look(): Unit = {
		manualUpdate()
		super.look()
	}

	def manualUpdate() {
		if ( lastUpdate < 0 ) { lastUpdate = GLFW.glfwGetTime() }
		val rotHorizontal = deltaFromMappings(RotateLeft, RotateRight, 0.1f)
		val rotVertical = deltaFromMappings(RotateUp, RotateDown, 0.1f)
		val zoom = deltaFromMappings(ZoomIn, ZoomOut, 0.003f)

		val curTime = GLFW.glfwGetTime()
		val f = ((curTime - lastUpdate) / 0.0166666667).toFloat
		lastUpdate = curTime

		rotation = rotation.mul(new Quaternionf().rotateZ(rotHorizontal * f * 0.25f)).normalize()
		rotation = rotation.mul(new Quaternionf().rotateY(rotVertical * f * 0.25f)).normalize()

		z = (z + zoom).clamp(minZ, maxZ)
//		rotation = rotation.rotateAxis(rotHorizontal * f, up.x, up.y, up.z)
//		rotation = rotation.rotateAxis(rotVertical * f, ortho.x, ortho.y, ortho.z)
	}

	override def moveEyeTo(eye: ReadVec3f): Unit = {}

	override def setMoveSpeed(multiplier: ReadVec3f): Unit = {}

	override def keymapNamespace: String = GlobeCamera.namespace

}


object GlobeCamera {
	val RotateLeft = "rotateLeft"
	val RotateRight = "rotateRight"
	val RotateUp = "rotateUp"
	val RotateDown = "rotateDown"

	val ZoomIn = "zoomIn"
	val ZoomOut = "zoomOut"

	val namespace = "GlobeCamera"

	Keymap.register(namespace, RotateLeft, GLFW.GLFW_KEY_LEFT)
	Keymap.register(namespace, RotateRight, GLFW.GLFW_KEY_RIGHT)
	Keymap.register(namespace, RotateUp, GLFW.GLFW_KEY_UP)
	Keymap.register(namespace, RotateDown, GLFW.GLFW_KEY_DOWN)


	Keymap.register(namespace, ZoomIn, GLFW.GLFW_KEY_W)
	Keymap.register(namespace, ZoomOut, GLFW.GLFW_KEY_S)
}