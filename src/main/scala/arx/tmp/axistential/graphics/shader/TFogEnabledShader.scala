package arx.axistential.graphics.shader

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/5/13
 * Time: 10:52 AM
 */

import arx.core.vec.ReadVec3f
import arx.core.vec.Vec3f
import arx.engine.world.World
import arx.tmp.game.logic.world.data.LightData
import arx.graphics.shader.ArxShader

trait TFogEnabledShader extends ArxShader {
	def world : World
	def pov : TCamera

	val u_OneOverFogRange = uniform[Float](1.0f)
	val u_FogEnd = uniform[Float](1.0f)
	val u_LowColor = uniform[ReadVec3f](Vec3f.One)
	val u_HighColor = uniform[ReadVec3f](Vec3f.One)
	val u_VertexOriginOffsetZ = uniform[Float](0.0f)

	val v_lwc = varying[Float]
	val v_untransformedZ = varying[Float]

	lazy val lightData = world.aux[LightData]
	def compFogEnd = pov.viewDistance - 2.0f
	def fogStart = compFogEnd * BaseAnthologiconWorldShader.fogStartPcnt
	def compOneOverFogRange = 1.0f / (compFogEnd - fogStart)
	def compVertexOriginOffsetZ = pov.eye.z * -1.0f + 75.0f

	u_LowColor.set( lightData.lowSkyColor _ )
	u_HighColor.set( lightData.highSkyColor _ )
	u_FogEnd.set( compFogEnd _ )
	u_OneOverFogRange.set( compOneOverFogRange _ )
	u_VertexOriginOffsetZ.set( compVertexOriginOffsetZ _ )


	def fogVertexSection(unprojectedVarName:String) = s"""
			  |
			  |	v_lwc = length($unprojectedVarName);
			  |	v_untransformedZ = $unprojectedVarName.z + u_VertexOriginOffsetZ;
			""".stripMargin
	def fogFragmentSection(colorVariableToMix:String,writeTo:String) =
		s"""
		  |	float fogFactor = clamp((u_FogEnd - v_lwc) * u_OneOverFogRange, 0.0,1.0 );
		  |	vec3 fogColor = mix(u_LowColor,u_HighColor, clamp(v_untransformedZ * 0.00909, 0.0, 1.0) );
		  |	$writeTo = vec4(mix(fogColor,$colorVariableToMix.rgb,fogFactor),$colorVariableToMix.a);
		""".stripMargin
}
