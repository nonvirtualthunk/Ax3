package arx.eldr.application.testbeds

import org.lwjgl.glfw
import org.lwjgl.glfw.GLFW._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl._
import org.lwjgl.system.MemoryUtil._


object SlowUpdates {
	// The window handle
	var window = 0l

	def run(): Unit = {
		try {
			init()
			loop()

			// Release window and window callbacks
			glfwDestroyWindow(window)
		} finally {
			glfwTerminate()
		}
	}

	def init(): Unit = {
		// Initialize glfw
		if (!glfwInit()) {
			throw new IllegalStateException("Unable to initialize GLFW")
		}

		// Configure our window
		glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 2)
		glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 0)

		window = glfwCreateWindow(800,600, "Hello World!", NULL, NULL)

		// Get the resolution of the primary monitor
		val vidmode = glfwGetVideoMode(glfwGetPrimaryMonitor())
		// Make the OpenGL context current
		glfwMakeContextCurrent(window)
		// Enable v-sync
		glfwSwapInterval(1) // Neither 0 nor 1 make any significant difference
		// Make the window visible
		glfwShowWindow(window)
	}

	def loop(): Unit = {
		GL.createCapabilities()
		// Set the clear color
		glClearColor(0,0,0,1)
		glViewport(0,0,800,600)

		var lastUpdated = glfw.GLFW.glfwGetTime()

		// Run until the user indicates we should stop
		while (!glfwWindowShouldClose(window)) {
			glClear(GL_COLOR_BUFFER_BIT)

			val curTime = glfw.GLFW.glfwGetTime()
			val deltaSeconds = curTime - lastUpdated
			lastUpdated = curTime

			// If we have a gap of more than 2 frames, print out the delta
			if (deltaSeconds > (0.016666667 * 2.1)) {
				println("Long update time: " + deltaSeconds)
			}

			glfwSwapBuffers(window)
			glfwPollEvents()
		}
	}

	def main(args: Array[String]) {
		SlowUpdates.run()
	}
}