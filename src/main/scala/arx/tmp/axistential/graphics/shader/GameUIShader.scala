package arx.axistential.graphics.shader

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/5/13
 * Time: 10:51 AM
 */

import arx.Prelude._
import arx.core.mat.Mat4x4
import arx.core.mat.ReadMat4x4
import arx.core.vec.ReadVec2f
import arx.core.vec.ReadVec3f
import arx.core.vec.ReadVec4f
import arx.engine.world.World
import arx.graphics.shader.ArxShader
import arx.graphics.shader.ArxShader.Sampler

class GameUIShader protected (val world : World, val povFunc : () => TCamera) extends ArxShader with TFogEnabledShader {
	def pov = povFunc()

	val a_Vertex = attribute[ReadVec3f]
	val a_TexCoord = attribute[ReadVec2f]
	val a_Color = attribute[ReadVec4f]
	val a_BillboardOffset = attribute[ReadVec3f]

	val v_texCoord = varying[ReadVec2f]
	val v_color = varying[ReadVec4f]

	val u_ModelViewMatrix = uniform[ReadMat4x4](Mat4x4.Identity)
	val u_ProjectionMatrix = uniform[ReadMat4x4](Mat4x4.Identity)

	val u_Texture0 = uniform[Sampler](0)

	u_ModelViewMatrix.set( pov.viewMatrix _ )
	u_ProjectionMatrix.set( pov.projectionMatrix _ )

	@Language ("GLSL")
	def vertexShaderBody: String =
		s"""
		  |void main () {
		  | vec4 basePos = u_ModelViewMatrix * vec4(a_Vertex,1.0);
		  | basePos.xyz += a_BillboardOffset.xyz;
		  |
		  | ${fogVertexSection("basePos")}
		  |
		  | v_texCoord = a_TexCoord;
		  | v_color = a_Color;
		  | gl_Position = u_ProjectionMatrix * basePos;
		  |}
		""".stripMargin

	@Language ("GLSL")
	def fragmentShaderBody: String =
		s"""
		  | out vec4 o_FragColor;
		  |
		  | void main () {
		  | vec4 textureColor = texture(u_Texture0,v_texCoord);
		  | textureColor.a = step(0.8,textureColor.a);
		  |
		  | vec4 baseColor = textureColor * (v_color * 2.0);
		  |
		  | ${fogFragmentSection("baseColor","vec4 finalColor")}
		  | o_FragColor = finalColor;
		  | gl_FragDepth = gl_FragCoord.z + (1.0 - step(0.01,finalColor.a));
		  | }
		""".stripMargin
}

object GameUIShader {
	val fetch = memoize ( (world:World,graphicsEngine:GraphicsEngine) => {
		new GameUIShader(world,graphicsEngine.pov _)
	})
	def apply ( world : World , graphicsEngine:GraphicsEngine ) = {
		fetch(world,graphicsEngine)
	}
}

