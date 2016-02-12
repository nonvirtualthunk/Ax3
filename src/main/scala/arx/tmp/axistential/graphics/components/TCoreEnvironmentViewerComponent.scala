package arx.axistential.graphics.components

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 4/20/12
 * Time: 5:30 PM
 * Created by nonvirtualthunk
 */

import java.util.concurrent.locks.ReentrantReadWriteLock

import arx.application.Noto
import arx.application.TLoggingLevelProvider
import arx.axistential.graphics.components.environmentviewer.EnvironmentViewerComponent2.VBOContainer
import arx.axistential.graphics.components.environmentviewer.TEnvironmentViewerLayer
import arx.axistential.graphics.components.renderers.TEnvironmentRenderer
import arx.core.vec.coordinates.VoxelCoord
import arx.graphics._
import arx.graphics.shader.TShader
import org.lwjgl.opengl.GL11

trait TCoreEnvironmentViewerComponent extends GraphicsComponent {
	val textureBlock = new TextureBlock(2048,2048)
//	textureBlock.minFilter = GL11.GL_NEAREST_MIPMAP_NEAREST
	textureBlock.minFilter = GL11.GL_NEAREST
	textureBlock.magFilter = GL11.GL_NEAREST
	var renderTasks : List[() => Boolean] = Nil
//	var textureProviders : List[TTextureProvider] = Nil
	def renderers : List[TEnvironmentRenderer]
	def shader : TShader

	def foreachVisibleTalea[U] ( f : (VoxelCoord) => U )
	def addViewLayer ( layer : TEnvironmentViewerLayer ) : Int
	def viewLayers : Array[TEnvironmentViewerLayer]

	/**
	 * Does the actual draw calls for all in-view vbos for the given layer
	 * @param layer layer to draw
	 * @param usage solidify usage for vbos, GL_STATIC_DRAW, GL_STREAM_DRAW, etc
	 */
	def drawViewLayer ( layer : TEnvironmentViewerLayer, usage : Int )
	def markNeedsUpdate ( v : VoxelCoord , mask : Int )
	def window : SlidingWindow2[VBOContainer]
	def viewLock : ReentrantReadWriteLock
	def inView : Array[Boolean]
}

object GraphicsLogging extends TLoggingLevelProvider {
	var _loggingLevel = Noto.Info
	override def loggingLevel = _loggingLevel
	def loggingLevel_= ( l : Int ) { _loggingLevel = l }
}