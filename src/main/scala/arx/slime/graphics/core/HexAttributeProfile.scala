package arx.slime.graphics.core

/**
  * Created by IntelliJ IDEA.
  * User: nvt
  * Date: 8/19/13
  * Time: 12:04 PM
  * Created by nonvirtualthunk
  */

import arx.core.vec.ReadVec2f
import arx.core.vec.ReadVec4f
import arx.graphics.AttributeProfile
import arx.graphics.data.PointBuilder
import org.lwjgl.opengl.GL11._

object HexAttributeProfile extends AttributeProfile(
	List("vertex" -> (3,GL_FLOAT),"texCoord" -> (2,GL_FLOAT),"color" -> (4,GL_UNSIGNED_BYTE),"edge" -> (1,GL_FLOAT))) {
	val VertexAttribute = attributesByName("vertex")
	val TexCoordAttribute = attributesByName("texCoord")
	val ColorAttribute = attributesByName("color")
	val EdgeAttribute = attributesByName("edge")

	vertexAttributeIndex = VertexAttribute
	texCoordAttributeIndex = TexCoordAttribute

	def createPointBuilder() = new PointBuilder(this) {
		val voff = attributes(VertexAttribute).byteOffset
		val tcoff = attributes(TexCoordAttribute).byteOffset
		val coff = attributes(ColorAttribute).byteOffset
		val eoff = attributes(EdgeAttribute).byteOffset

		def setV(x:Float,y:Float,z:Float): Unit = {
			bytes.position(voff)
			bytes.putFloat(x)
			bytes.putFloat(y)
			bytes.putFloat(z)
		}

		def setTC(v : ReadVec2f): Unit = {
			bytes.position(tcoff)
			bytes.putFloat(v.x)
			bytes.putFloat(v.y)
		}

		def setTC(x:Float,y:Float): Unit = {
			bytes.position(tcoff)
			bytes.putFloat(x)
			bytes.putFloat(y)
		}

		def setC(r:Float,g:Float,b:Float,a:Float): Unit = {
			bytes.position(coff)
			bytes.put((r*255).toByte)
			bytes.put((g*255).toByte)
			bytes.put((b*255).toByte)
			bytes.put((a*255).toByte)
		}
		def setC(rgba : ReadVec4f): Unit = {
			setC(rgba.r,rgba.g,rgba.b,rgba.a)
		}

		def setE(edge : Float) = {
			bytes.position(eoff)
			bytes.putFloat(edge)
		}
	}
}