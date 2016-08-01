package arx.eldr.graphics.environment

/**
 * TODO: Add javadoc
 */

import arx.core.vec.ReadVec2f
import arx.graphics.AttributeProfile
import arx.graphics.data.PointBuilder
import org.lwjgl.opengl.GL11._

object WorldAttributeProfile extends AttributeProfile(
	List(
		"Vertex" ->(3, GL_FLOAT),
		"TexCoord" ->(2, GL_FLOAT),
		"LocalLight" ->(4, GL_UNSIGNED_BYTE),
		"GlobalLight" ->(4, GL_UNSIGNED_BYTE))
) {
	val VertexAttribute = attributesByName("Vertex")
	val TexCoordAttribute = attributesByName("TexCoord")
	val LocalLightAttribute = attributesByName("LocalLight")
	val GlobalLightAttribute = attributesByName("GlobalLight")

	vertexAttributeIndex = VertexAttribute
	texCoordAttributeIndex = TexCoordAttribute


	def createPointBuilder() = new PointBuilder(this) {
		val voff = attributes(VertexAttribute).byteOffset
		val tcoff = attributes(TexCoordAttribute).byteOffset
		val lloff = attributes(LocalLightAttribute).byteOffset
		val gloff = attributes(GlobalLightAttribute).byteOffset

		def setV(x: Float, y: Float, z: Float): Unit = {
			bytes.position(voff)
			bytes.putFloat(x)
			bytes.putFloat(y)
			bytes.putFloat(z)
		}

		def setTC(v: ReadVec2f): Unit = {
			bytes.position(tcoff)
			bytes.putFloat(v.x)
			bytes.putFloat(v.y)
		}

		def setTC(x: Float, y: Float): Unit = {
			bytes.position(tcoff)
			bytes.putFloat(x)
			bytes.putFloat(y)
		}

		def setLL(r: Float, g: Float, b: Float, a: Float): Unit = {
			bytes.position(lloff)
			bytes.put((r * 127).toByte)
			bytes.put((g * 127).toByte)
			bytes.put((b * 127).toByte)
			bytes.put((a * 127).toByte)
		}

		def setGL(r: Float, g: Float, b: Float, a: Float): Unit = {
			bytes.position(gloff)
			bytes.put((r * 127).toByte)
			bytes.put((g * 127).toByte)
			bytes.put((b * 127).toByte)
			bytes.put((a * 127).toByte)
		}
	}
}