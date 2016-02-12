package arx.axistential.graphics.components.renderers

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 4/24/12
 * Time: 11:41 AM
 * Created by nonvirtualthunk
 */

import arx.core.vec.ReadVec2f
import arx.engine.world.World
import arx.graphics.AttributeProfile
import arx.graphics.TextureBlock
import org.lwjgl.opengl.GL11._

import scala.collection.mutable

trait AnthologiconBaseEnvironmentRenderer {
	val attribProfile = AttributeProfile("vertex" -> (3,GL_FLOAT), "texCoords" -> (2,GL_FLOAT) ,
														"localLight" -> (4,GL_UNSIGNED_BYTE), "globalLight" -> (4,GL_UNSIGNED_BYTE))
	val V = attribProfile.attributesByName("vertex")
	val LL = attribProfile.attributesByName("localLight")
	val GL = attribProfile.attributesByName("globalLight")
	val T = attribProfile.attributesByName("texCoords")

	/** Tex coords are in q,material,k order*/
	val cachedTexCoords = new mutable.HashMap[Int,Array[Array[Array[ReadVec2f]]]]

	val textureProvider = ReflectionAssistant.provideInstanceOf[TGameEntityGraphicsInfoProvider]

	var drawingContexts = new mutable.HashMap[(TextureBlock,Int),EnvironmentDrawingContext]()

	def buildDrawingContext(textureBlock:TextureBlock,world:World) = {
		EnvironmentDrawingContext.fromWorld(world,textureBlock)
	}
}