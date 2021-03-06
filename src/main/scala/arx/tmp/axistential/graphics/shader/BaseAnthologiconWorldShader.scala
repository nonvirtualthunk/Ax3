package arx.axistential.graphics.shader

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/11/13
 * Time: 11:45 AM
 * Created by nonvirtualthunk
 */

import arx.application.Application
import arx.axistential.game.data.world.CloudData
import arx.core.Moddable
import arx.core.function.FAF
import arx.core.mat.Mat4x4
import arx.core.mat.ReadMat4x4
import arx.core.vec.Vec2f
import arx.core.vec.Vec3f
import arx.core.vec.Vec4f
import arx.engine.world.World
import arx.tmp.game.logic.world.data.LightData
import arx.graphics.shader.ArxShader
import arx.graphics.shader.ArxShader.Sampler

abstract class BaseAnthologiconWorldShader(env:World,pov:Moddable[TCamera]) extends ArxShader {
	val Vertex = attribute[Vec3f]
	val TexCoord = attribute[Vec2f]
	val LocalLightData = attribute[Vec4f]
	val GlobalLightData = attribute[Vec4f]
	val BillboardOffset = attribute[Vec2f]
	BillboardOffset.active = billboard _

	val vertexOriginOffset = uniform[Vec3f](Vec3f(0.0f,0.0f,0.0f))
	val zcutoff = uniform[Float](0.0f)
	val fogEnd = uniform[Float](100.0f)
	val oneOverFogRange = uniform[Float](0.01f)
	val lowColor = uniform[Vec3f](Vec3f.One)
	val highColor = uniform[Vec3f](Vec3f.One)
	val globalLightColor = uniform[Vec3f](Vec3f.One)
	val globalLightModifier = uniform[Float](1.0f)
	val ModelViewMatrix = uniform[ReadMat4x4]( Mat4x4.Identity )
	val ProjectionMatrix = uniform[ReadMat4x4]( Mat4x4.Identity )
	val tex0 = uniform[Sampler](0)
	val cloudTexture = uniform[Sampler](1)
	val cloudOffset = uniform[Float](0.0f)
	val cloudUniforms = cloudTexture :: cloudOffset :: Nil
	cloudUniforms.foreach ( _.active = cloudsActive _ )

	zcutoff.active = cutoffActive _

	//+=============================+ Cutaway +=============================+
	val cutawayXYThreshold = uniform[Float](20.0f)
	val cutawayZThreshold = uniform[Float](20.0f)
	val closeFactor = varying[Float]

	val cutawayVariables = closeFactor :: cutawayXYThreshold :: cutawayZThreshold :: Nil
	cutawayVariables.foreach( _.active = cutaway _ )

	//+=============================+ Default Values +=============================+
	val lightData = env.aux[LightData]

	cutawayXYThreshold.set( () => pov.cutawayXYThreshold )
	cutawayZThreshold.set( () => pov.cutawayZThreshold )

	zcutoff.set( () => pov.zCutoff + 1.0f )

	globalLightColor.set( FAF(lightData.globalLightColor.apply _,0) )
	globalLightModifier.set( FAF(lightData.globalLightStrength.apply _,0) )

	def compFogEnd = pov.viewDistance - 2.0f
	fogEnd.set( compFogEnd _ )
	def fogStart = compFogEnd * BaseAnthologiconWorldShader.fogStartPcnt
	oneOverFogRange.set( () => 1.0f / (compFogEnd - fogStart) )

	vertexOriginOffset.set( () => Vec3f(pov.eye * -1.0f) )

	lowColor.set( lightData.lowSkyColor _ )
	highColor.set( lightData.highSkyColor _ )

	cloudTexture.set(Sampler(1))
	cloudOffset.set( env.auxData[CloudData].cloudOffset _ )

	//+=============================+ Common +=============================+
	val untransformedXY = varying[Vec2f]
	val cutf = varying[Float]

	val localLightV = varying[Vec4f]
	val globalLightV = varying[Vec4f]
	val texCoordV = varying[Vec2f]

	//+=============================+ Vertex Heavy +=============================+
	val fogFactor = varying[Float]
	val fogColor = varying[Vec3f]
	val vertexHeavyVaryings = fogFactor :: fogColor :: Nil
	vertexHeavyVaryings.foreach( _.active = vertexHeavy _ )

	//+=============================+ Fragment Heavy +=============================+
	val lwc = varying[Float]
	val untransformedZ = varying[Float]
	val fragmentHeavyVaryings = lwc :: untransformedZ :: Nil
	fragmentHeavyVaryings.foreach( _.active = fragmentHeavy _ )

	def vertexShaderBody = vertexShaderMain

	var vertexHeavy = false
	var cloudsActive = false
	var hasTransparency = false
	var cutaway = false
	var billboard = false
	var cutoffActive = false
	var breakUpFog = false

	def mayDiscard = cutaway || hasTransparency || cutoffActive
	def noTransparency = ! hasTransparency

	def withZCutoff ( zc : Moddable[Float] ) = { zcutoff.set(zc); this }
	def withCutoffActive ( enable : Boolean ) = { cutoffActive = enable; this }
	def withCutawayActive ( enable : Boolean ) = { cutaway = enable; this }
	def withCutawayXYThreshold ( v : Moddable[Float] ) = { cutawayXYThreshold.set(v); this }
	def withCutawayZThreshold ( f : Moddable[Float] ) = { cutawayZThreshold.set(f); this }

	def fragmentHeavy = ! vertexHeavy

	@Language("GLSL")
	def vertexShaderMain : String =
		"""
		  float rand(vec2 co){
		      return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
		  }

		  void main () {
			  vec4 unpersp = (ModelViewMatrix * vec4(Vertex,1.0));
			#if breakUpFog
		  		float lwc = length( unpersp.xyz ) + cos( length(unpersp.xy) * 0.1 ) * 6.0;
			#else
		  		float lwc = length( unpersp.xyz );
			#end

			#if active BillboardOffset
				unpersp.xy += BillboardOffset.xy;
			#end
			  untransformedXY = Vertex.xy;
			  float untransformedZ = Vertex.z + vertexOriginOffset.z + 75.0;

			#if vertexHeavy
			  fogFactor = clamp((fogEnd - lwc) * oneOverFogRange, 0.0,1.0 );
			  fogColor = mix(lowColor,highColor, clamp(untransformedZ * .00909,0.0,1.0) );
			#end

			#if cutaway
		  		float xyDist = length((Vertex.xy + vertexOriginOffset.xy));
		      float zDist = abs(Vertex.z + vertexOriginOffset.z);
				closeFactor = max(clamp(floor(xyDist / cutawayXYThreshold),0.0,1.01) , clamp(floor(zDist / cutawayZThreshold),0.0,1.01));
			#end

			#if cutoffActive
			  cutf = min(0.0,Vertex.z - zcutoff) * -1.0;
			#end

			  texCoordV = TexCoord;
			  localLightV = LocalLightData * 2.0;   // (LightColor.r,LightColor.g,LightColor.b,LocalLightStrength)
			  globalLightV = GlobalLightData * 2.0; // (MatColor.r,MatColor.g,MatColor.b,GlobalLightStrength)

			  gl_Position = ProjectionMatrix * unpersp;
		  }
		"""


	def fragmentShaderBody = fragmentShaderMain

	@Language("GLSL")
	def fragmentShaderMain =
		"""
		  	out vec4 o_FragColor;

			void main (){
			#if mayDiscard
				bool shouldDiscard = false;
			#end

		  	#if hasTransparency
		  		vec4 textureColor = texture(tex0,texCoordV);
		      textureColor.a = step(0.65,textureColor.a);
				shouldDiscard = textureColor.a == 0.0;
		  	#end

		   #if cutaway
				shouldDiscard = shouldDiscard || (closeFactor <= 0.0);
		   #elseif cutoffActive
				shouldDiscard = shouldDiscard || (cutf < 0.001);
		   #end

			#if mayDiscard
				if ( shouldDiscard ) { discard; }
			#end

			#if noTransparency
		  		vec4 textureColor = texture(tex0,texCoordV);
			#end

			#if cloudsActive
		  		float cloudMultiplier = texture(cloudTexture,(untransformedXY + 512.0 + cloudOffset) * 0.0009765625).r;
		  		float globalLightValue = globalLightV.a * globalLightModifier * cloudMultiplier;
			#else
				float globalLightValue = globalLightV.a * globalLightModifier;
			#end



		      float natProportion = globalLightValue / max(globalLightValue + localLightV.a,0.001);

			// take a mix between the local color and the global color determined by the relative strength of each
		      vec3 mixedColor = mix(localLightV.rgb,globalLightColor,natProportion);
			// scale the color (coneptually, the hue) such that the highest of (r,g,b) has a value of 1
				float maxElem = max(max(mixedColor.r,mixedColor.g),mixedColor.b);
	 			float scaleBy = 1.0 / maxElem;
			// further, scale it by the actual strength of the light
		  		vec3 rawLightColor = mixedColor * scaleBy * max(globalLightValue,localLightV.a);
		      vec4 lightColor = vec4(globalLightV.rgb * rawLightColor,1.0);
			// mix with the texture color for the final-ish result
		      vec4 baseColor = textureColor * lightColor;

			#if fragmentHeavy
		      float fogFactor = clamp((fogEnd - lwc) * oneOverFogRange, 0.000001,1.0 );
		      vec3 fogColor = mix(lowColor,highColor, clamp(untransformedZ * .00909,0.0,1.0) );
			#end

		      vec4 finalColor = vec4(mix(fogColor, baseColor.rgb, fogFactor),baseColor.a);

		      o_FragColor = finalColor;
			}
		"""
}

class AnthologiconWorldShader(env:World,pov:Moddable[TCamera]) extends BaseAnthologiconWorldShader(env,pov) {
}

object BaseAnthologiconWorldShader {
	var fogStartPcnt = 0.7f
}

class AnthologiconBillboardShader(env:World,pov:Moddable[TCamera]) extends BaseAnthologiconWorldShader(env,pov) {
	billboard = true
	hasTransparency = true
}
class AnthologiconWorldCutawayShader(env:World,pov:Moddable[TCamera]) extends BaseAnthologiconWorldShader(env,pov) {
	cutaway = true
}

object BaseAnthWorldShaderApp extends Conflux {
	override def initGL() {
		super.initGL()

		val env = new World
		env.auxData[LightData]
		val pov = new Camera

		val shader = new AnthologiconWorldShader(env,pov)

		shader.bind()
	}

	def main ( args : Array[String] ) {
		Application.start(this)

	}
}