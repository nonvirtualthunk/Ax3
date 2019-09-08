package arx.tyche.graphics.core

import arx.graphics.AttributeProfile
import org.lwjgl.opengl.GL11.{GL_FLOAT, GL_UNSIGNED_BYTE, GL_BYTE}

object TyAttributeProfile extends AttributeProfile(List(
	"vertex" -> (3, GL_FLOAT),
	"texCoord" -> (2, GL_FLOAT),
	"color" -> (4, GL_UNSIGNED_BYTE),
	"lightPcnt" -> (1, GL_FLOAT),
	"lightColor" -> (4,GL_UNSIGNED_BYTE),
	"normal" -> (4, GL_FLOAT),
	"billboard" -> (2, GL_FLOAT))
) {
	val VertexAttribute = attributesByName("vertex")
	val TexCoordAttribute = attributesByName("texCoord")
	val ColorAttribute = attributesByName("color")
	val LightPcntAttribute = attributesByName("lightPcnt")
	val LightColorAttribute = attributesByName("lightColor")
	val Normal = attributesByName("normal")
	val BillboardAttribute = attributesByName("billboard")


	val V = VertexAttribute
	val TC = TexCoordAttribute
	val C = ColorAttribute
	val LP = LightPcntAttribute
	val LC = LightColorAttribute
	val N = Normal
}