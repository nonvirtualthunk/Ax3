package arx.axistential.graphics.shader

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/11/13
 * Time: 6:34 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.axistential.game.data.world.CloudData
import arx.axistential.game.data.world.ScaleData
import arx.core.mat.Mat4x4
import arx.core.mat.ReadMat4x4
import arx.core.vec.Vec2f
import arx.core.vec.Vec3f
import arx.core.vec.Vec4f
import arx.engine.world.World
import arx.tmp.game.logic.world.data.LightData
import arx.graphics.shader.ArxShader

class CloudShader(env:World,pov :TCamera) extends ArxShader {
	val Vertex = attribute[Vec3f]
	val Color = attribute[Vec4f]

	val untransformedXY = varying[Vec2f]
	val color = varying[Vec2f]
	val lwc = varying[Float]

	val globalLightColor = uniform[Vec3f](Vec3f.One)
	val cloudTexture = uniform[ArxShader.Sampler](1)
	val cloudOffset = uniform[Float](0.0f)
	val eyeOffsetXY = uniform[Vec2f](Vec2f.Zero)
	val fogEnd = uniform[Float](100.0f)
	val oneOverFogRange = uniform[Float](1.0f)
	val ModelViewMatrix = uniform[ReadMat4x4]( Mat4x4.Identity )
	val ProjectionMatrix = uniform[ReadMat4x4]( Mat4x4.Identity )

	cloudOffset.set(env.auxData[CloudData].cloudOffset _)
	globalLightColor.set( () => env.aux[LightData].globalLightColor.apply(0) * env.aux[LightData].globalLightStrength(0) )

	def cloudHeight = env.aux[ScaleData].CloudLevel.inVoxels
	def compFogEnd = sqrtf(pov.viewDistance * pov.viewDistance - cloudHeight * cloudHeight)
	fogEnd.set( compFogEnd _ )
	def fogStart = compFogEnd * BaseAnthologiconWorldShader.fogStartPcnt
	oneOverFogRange.set( () => 1.0f / (compFogEnd - fogStart) )
	eyeOffsetXY.set( pov.eye.xy _ )

//	hyp = viewdist
//	opp = cloudHeight
//	hyp*hyp - opp*opp = adj*adj
//
	@Language("GLSL")
	var vertexShaderBody: String =
	"""
			void main () {
				vec4 unpersp = ModelViewMatrix * vec4(Vertex,1.0);
		  		gl_Position = ProjectionMatrix * unpersp;
				untransformedXY = Vertex.xy;
				color = Color.xy;
				lwc = length(unpersp.xyz);
			}
	"""

	@Language("GLSL")
	var fragmentShaderBody: String =
		"""
		   out vec4 o_FragColor;
			void main () {
				//float lwc = length( untransformedXY );
				float baseCloudStrength = 1.4 - cloudAccess;
				float cloudStrength = baseCloudStrength * baseCloudStrength;
				float fogFactor = clamp((fogEnd - lwc) * oneOverFogRange,0.0,1.0);
				float lightness = color.x;
				float weight = color.y;
//				gl_FragDepth = gl_FragCoord.z + (1.0 - step(0.01,cloudStrength * weight));
		  		o_FragColor = vec4(globalLightColor * lightness,cloudStrength * fogFactor * weight);
			}
		"""
	fragmentShaderBody = fragmentShaderBody.replace("cloudAccess",CloudShader.accessCloudTextureSnippet)
}

object CloudShader {
	val accessCloudTextureSnippet = "texture(cloudTexture,(untransformedXY + eyeOffsetXY + 512.0 + cloudOffset) / 1024.0).r"
}
