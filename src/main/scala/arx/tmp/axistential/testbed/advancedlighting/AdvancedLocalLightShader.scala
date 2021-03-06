package arx.axistential.testbed.advancedlighting

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 4/22/14
 * Time: 7:20 AM
 */

import arx.core.Moddable
import arx.core.function.FAF
import arx.core.mat.Mat4x4
import arx.core.mat.ReadMat4x4
import arx.core.vec._
import arx.engine.world.World
import arx.tmp.game.logic.world.data.LightData
import arx.graphics.AttributeProfile
import arx.graphics.shader.ArxShader
import arx.graphics.shader.ArxShader.Sampler
import org.lwjgl.opengl.GL11

class AdvancedLocalLightShader(env:World,pov:Moddable[TCamera]) extends ArxShader {
	autoUpdate = true

	val a_Vertex = attribute[Vec3f]
	val a_TexCoord = attribute[Vec2f]
	val a_GlobalLightData = attribute[Vec4f]
	val a_LocalLightStrengths = attribute[Vec4i]
	val a_LocalLightIndices = attribute[Vec4i]
	val a_SideIndex = attribute[Int]
	val a_Unused = attribute[Vec3f]

	val u_GlobalLightColor = uniform[Vec3f](Vec3f.One)
	val u_GlobalLightModifier = uniform[Float](1.0f)
	val ModelViewMatrix = uniform[ReadMat4x4]( Mat4x4.Identity )
	val ProjectionMatrix = uniform[ReadMat4x4]( Mat4x4.Identity )
	val u_MainTexture = uniform[Sampler](0)
	val u_LocalLightPositions = uniform[Vector[ReadVec3f]](Vector(),32)
	val u_LocalLightColors = uniform[Vector[ReadVec3f]](Vector(),32)

	//+=============================+ Default Values +=============================+
	val lightData = env.aux[LightData]

	u_GlobalLightColor.set( FAF(lightData.globalLightColor.apply,0) )
	u_GlobalLightModifier.set( FAF(lightData.globalLightStrength.apply,0) )

	//+=============================+ Common +=============================+
	val v_LocalLightStrengths = varying[Vec4i]
	val v_LocalLightIndices = varying[Vec4i]
	val v_GlobalLightData = varying[Vec4f]
	val v_TexCoord = varying[Vec2f]
	val v_SideIndex = varying[Int]
	val v_UnperspPosition = varying[Vec3f]

	def vertexShaderBody = vertexShaderMain
	def fragmentShaderBody = fragmentShaderMain

	@Language("GLSL")
	def vertexShaderMain : String =
		"""
void main () {
	vec4 unpersp = (ModelViewMatrix * vec4(a_Vertex,1.0));

	v_TexCoord = a_TexCoord;
	v_LocalLightIndices = a_LocalLightIndices;
	v_LocalLightStrengths = a_LocalLightStrengths * 2.0;
	v_GlobalLightData = a_GlobalLightData * 2.0;
	v_SideIndex = a_SideIndex;
	v_UnperspPosition = a_Vertex.xyz;

	gl_Position = ProjectionMatrix * unpersp;
}
		"""

	@Language("GLSL")
	def fragmentShaderMain =
		"""
out vec4 o_FragColor;

const vec3 c_SideNormals[6] = vec3[6](
	vec3(-1.0,0.0,0.0),
	vec3(0.0,-1.0,0.0),
	vec3(0.0,0.0,-1.0),
	vec3(1.0,0.0,0.0),
	vec3(0.0,1.0,0.0),
	vec3(0.0,0.0,1.0)
);

void main (){
	vec4 textureColor = texture(u_MainTexture,v_TexCoord);

	float globalLightValue = v_GlobalLightData.a * u_GlobalLightModifier;

   vec3 normal = c_SideNormals[int(v_SideIndex)];
	vec3 lightVector = normalize(u_LocalLightPositions[0] - v_UnperspPosition);
//	float lightMult = ((dot(lightVector,normal) + 1.0) * 0.5 * 0.5 + 0.5) * v_LocalLightStrengths.x;
	float lightMult = v_LocalLightStrengths.x;

	vec4 baseColor = textureColor;
//	baseColor.rgb *= globalLightValue;
	baseColor.rgb *= lightMult;

	vec4 finalColor = baseColor;

	o_FragColor = finalColor;
}
		"""
}

import org.lwjgl.opengl.GL11._
object AdvancedLocalLightAttributeProfile extends AttributeProfile(
	"a_Vertex" -> (3,GL_FLOAT) ::
	"a_TexCoord" -> (2,GL_FLOAT) ::
	"a_GlobalLightData" -> (4,GL_UNSIGNED_BYTE) ::
	"a_LocalLightStrengths" -> (4,GL_UNSIGNED_BYTE) ::
	"a_LocalLightIndices" -> (4,GL_UNSIGNED_BYTE) ::
	"a_SideIndex" -> (1,GL_BYTE) ::
	"a_Unused" -> (3,GL_UNSIGNED_BYTE) ::
	Nil
) {
	val Vertex = attributesByName("a_Vertex")
	val TexCoord = attributesByName("a_TexCoord")
	val SideIndex = attributesByName("a_SideIndex")
	attributes(SideIndex).rawInteger = true
	val GlobalLightData = attributesByName("a_GlobalLightData")
	val LocalLightStrengths = attributesByName("a_LocalLightStrengths")
	val LocalLightIndices = attributesByName("a_LocalLightIndices")
}
//
//trait TBillboardOffsetAttributeProfile extends AttributeProfile {
//	def BillboardOffset : Int
//}
//
//trait TTexturedBillboardOffsetAttributeProfile extends AttributeProfile {
//
//}