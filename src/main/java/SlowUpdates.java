import org.lwjgl.glfw.GLFWVidMode;
import org.lwjgl.opengl.GL;

import static org.lwjgl.glfw.GLFW.*;
import static org.lwjgl.opengl.GL11.*;

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/31/16
 * Time: 5:16 PM
 */
public class SlowUpdates {
    // The window handle
    long window = 0l;

    public void run() {
        try {
            init();
            loop();

            // Release window and window callbacks
            glfwDestroyWindow(window);
        } finally {
            glfwTerminate();
        }
    }

    public void init() {
        // Initialize glfw
        if (!glfwInit()) {
            throw new IllegalStateException("Unable to initialize GLFW");
        }

        // Configure our window
        glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
        glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
        glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
        glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 1);

        window = glfwCreateWindow(800,600, "Hello World!", 0L, 0L);

        // Get the resolution of the primary monitor
        GLFWVidMode vidmode = glfwGetVideoMode(glfwGetPrimaryMonitor());
        // Make the OpenGL context current
        glfwMakeContextCurrent(window);
        // Enable v-sync
        glfwSwapInterval(1); // Neither 0 nor 1 make any significant difference
        // Make the window visible
        glfwShowWindow(window);
    }

    public void loop() {
        GL.createCapabilities();
        // Set the clear color
        glClearColor(0,0,0,1);
        glViewport(0,0,800,600);

        double lastUpdated = glfwGetTime();

        // Run until the user indicates we should stop
        while (!glfwWindowShouldClose(window)) {
//            glClear(GL_COLOR_BUFFER_BIT);

            double curTime = glfwGetTime();
            double deltaSeconds = curTime - lastUpdated;
            lastUpdated = curTime;

            // If we have a gap of more than 2 frames, print out the delta
            if (deltaSeconds > (0.016666667 * 2.1)) {
                System.out.println("Long update time: " + deltaSeconds);
            }

            glfwSwapBuffers(window);
            glfwPollEvents();
        }
    }

    public static void main(String[] args) {
        new SlowUpdates().run();
    }
}
