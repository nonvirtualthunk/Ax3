package arx.axistential.graphics.components.weather

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/2/13
 * Time: 2:00 PM
 */

import arx.Prelude._
import arx.axistential.game.components.Cloud
import arx.axistential.game.data.world.ScaleData
import arx.axistential.game.data.world.TopologicalData
import arx.axistential.graphics.shader.BaseAnthologiconWorldShader
import arx.core.mat.Mat4x4
import arx.core.mat.ReadMat4x4
import arx.core.mathutil.CircleDistribution
import arx.core.vec._
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.world.World
import arx.tmp.game.logic.world.data.LightData
import arx.tmp.game.procedural.generators.CloudImageGenerator
import arx.tmp.game.procedural.generators.ImageTransformations
import arx.graphics.shader.ArxShader
import arx.graphics.traits.TRenderTarget
import arx.graphics.AttributeProfile
import arx.graphics.Image
import org.lwjgl.opengl.GL11

import scala.collection.mutable
import scalaxy.loops._

class WeatherGraphicsComponent extends DynamicGraphicsComponent {
	lazy val shader = new PrecipitationShader(world,graphicsEngine.pov)
	val AP = PrecipitationShader.AttributeProfile

	val snowTexture = image("axis/entities/materials/textures/snow_precipitation.png")
	PNGTransparentColorizer.colorizeTransparency(snowTexture)

	val cloudImages = new mutable.HashMap[Cloud,Image]

	case class PrecipitationSheet ( var offset : ReadVec3f, var zScroll : Float , var scrollSpeed : Float )
	val precipitationSheets = new mutable.HashMap[Cloud,List[PrecipitationSheet]]()
	val cloudZs = new mutable.HashMap[Cloud,Float]

	def createSheetsForCloud(cloud: Cloud): List[PrecipitationSheet] = {
		val sheets = for ( i <- 0 until 10 ) yield {
			val offset2d = CircleDistribution.generatePoint(cloud.radius.inVoxels)
			PrecipitationSheet(Vec3f(offset2d.x,offset2d.y,0.0f), rand(0.0f,1.0f) , ND(1.0f,0.1f) )
		}
		sheets.toList
	}

	def createCloudImage ( cloud : Cloud ) = {
		val baseImage = CloudImageGenerator.generate(128,128,1.0f,3)
		ImageTransformations.rescaleBrightness(baseImage,0.85f,1.0f)
		baseImage
	}

	def cloudPosition ( cloud : Cloud ) = {
		val z = cloudZs.getOrElseUpdate(cloud,world.aux[ScaleData].CloudLevel.inVoxels - 15.0f + rand(-10.0f,10.0f))

		val base = ObjectCoord(cloud.position.x - VoxelCoord.Center.x,cloud.position.y - VoxelCoord.Center.y,z)
		val dz = (distance(base.xy,pov.eye.xy) / pov.viewDistance) * 80.0f

		base.minusZ(dz)
	}

	var lastGameEngineTime = -1.0f
	protected def update(f: Float): Unit = {

		val bucket = getBucket
		if ( pov.eye.z > world.aux[ScaleData].CloudLevel.inVoxels ) {
			bucket.drawOrder = GraphicsComponentDrawOrder.AbsolutelyFinal * 9
		} else {
			bucket.drawOrder = GraphicsComponentDrawOrder.AbsolutelyFinal * 11
		}

		bucket.cull = false
		bucket.depthTest = true
		bucket.depthWrites = false
		bucket.depthFunc = GL11.GL_LEQUAL
		bucket.textureBlock.magFilter = GL11.GL_NEAREST

		if ( lastGameEngineTime < 0  ) { lastGameEngineTime = world.time.inSeconds }
		val newGameEngineTime = world.time.inSeconds
		val deltaT = newGameEngineTime - lastGameEngineTime
		lastGameEngineTime = newGameEngineTime

		val snowTC = bucket.textureBlock(snowTexture)

		val clouds = world.entitiesOfType[Cloud]
		val cloudsAndPositions = clouds.toList.zip( clouds.toList.map( cloudPosition ) ).toList.sortBy( _._2.z * -1)
		for ( (cloud,cloudPos) <- cloudsAndPositions ) {
			val cloudImage = cloudImages.getOrElseUpdate(cloud, createCloudImage(cloud) )
			quad(bucket,cloudPos,Vec2f(cloud.radius.inVoxels*3.0f),Vec3f.UnitX,Vec3f.UnitY,bucket.textureBlock(cloudImage),Vec4f.One)
		}

		for ( (cloud,cloudPos) <- cloudsAndPositions ) {
			val sheets = precipitationSheets.getOrElseUpdate(cloud,createSheetsForCloud(cloud))

			for ( sheet <- sheets ){
				val partialTC = TexCoordUtils.cut(snowTC,0.0f,0.0f,1.0f,sheet.zScroll)
				billboardQuad(bucket,cloudPos.minusZ(sheet.zScroll * 20.0f) + sheet.offset,Vec2f(20.0f,sheet.zScroll * 20.0f),partialTC,Vec4f.One)
				val h = cloudPos.z.toInt - world.aux[TopologicalData].heightmap(cloudPos.toVoxelCoord)
				for ( z <- 20 until 160 by 20 optimized ) {
					/*.plusX(sinf((sheet.zScroll * 20.0f + z) * 0.025f) * 4.0f)*/
					billboardQuad(bucket,cloudPos.minusZ(sheet.zScroll * 20.0f + z) + sheet.offset,Vec2f(20.0f),snowTC,Vec4f.One)
				}
//				sheet.zScroll += deltaT * .25f * sheet.scrollSpeed
				sheet.zScroll += 0.016667f * f * .25f * sheet.scrollSpeed
				if ( sheet.zScroll > 1.0f ) { sheet.zScroll -= 1.0f }
			}
		}
	}

	def bucketIdentifier: Symbol = 'weather
	def bucketRequirements = RenderBucketRequirements(AP,shader)



	def billboardQuad (bucket : TRenderTarget, position : ReadVec3f, dimensions : ReadVec2f, texCoords : Array[ReadVec2f], color : ReadVec4f) {
		val vi = bucket.incrementVertexOffset(4)
		val ii = bucket.incrementIndexOffset(6)

		for ( i <- 0 until 4 optimized ) {
			bucket.vbo.setA(AP.VertexAttribute,vi + i,position.x,position.y,position.z + (if ( i < 2 ) { 0.0f } else { dimensions.y }))
			bucket.vbo.setAbf(AP.ColorAttribute,vi + i,color.r,color.g,color.b,color.a,255)
			bucket.vbo.setA(AP.TexCoordAttribute,vi + i,texCoords(i))
			bucket.vbo.setA(AP.BillboardOffsetAttribute,vi + i,Cardinals.unitBillboardOffsets(i).x * dimensions.x,0.0f)
		}
		bucket.vbo.setIQuad(ii,vi)
	}
	def quad (bucket : TRenderTarget, position : ReadVec3f, dimensions : ReadVec2f, forward : ReadVec3f, ortho : ReadVec3f, texCoords : Array[ReadVec2f], color : ReadVec4f) {
		val vi = bucket.incrementVertexOffset(4)
		val ii = bucket.incrementIndexOffset(6)

		for ( i <- 0 until 4 optimized ) {
			bucket.vbo.setA(AP.VertexAttribute,vi + i,position + forward * (Cardinals.centeredCubePoints(Top)(i).x * dimensions.x) + ortho * (Cardinals.centeredCubePoints(Top)(i).y * dimensions.y))
			bucket.vbo.setAbf(AP.ColorAttribute,vi + i,color.r,color.g,color.b,color.a,255)
			bucket.vbo.setA(AP.TexCoordAttribute,vi + i,texCoords(i))
			bucket.vbo.setA(AP.BillboardOffsetAttribute,vi + i,0.0f,0.0f)
		}
		bucket.vbo.setIQuad(ii,vi)
	}
}


class PrecipitationShader(world:World, pov : TCamera) extends ArxShader {
	val Vertex = attribute[Vec3f]
	val Color = attribute[Vec4f]
	val TexCoord = attribute[Vec2f]
	val BillboardOffset = attribute[Vec2f]

	val v_color = varying[Vec4f]
	val v_texCoord = varying[Vec2f]
	val v_lwc = varying[Float]
	val v_untransformedZ = varying[Float]

	val globalLightColor = uniform[Vec3f](Vec3f.One)
	val precipitationTexture = uniform[ArxShader.Sampler](0)
	val fogEnd = uniform[Float](100.0f)
	val oneOverFogRange = uniform[Float](1.0f)
	val ModelViewMatrix = uniform[ReadMat4x4]( Mat4x4.Identity )
	val ProjectionMatrix = uniform[ReadMat4x4]( Mat4x4.Identity )
	val VertexZOffset = uniform[Float]( 0.0f )
	val lowColor = uniform[Vec3f](Vec3f.One)
	val highColor = uniform[Vec3f](Vec3f.One)


	val lightData = world.aux[LightData]

	VertexZOffset.set( () => -pov.eye.z )
	lowColor.set( lightData.lowSkyColor _ )
	highColor.set( lightData.highSkyColor _ )

	// TODO : This seems like a bad idea...why are we doing this here?
	globalLightColor.set( () => world.aux[LightData].globalLightColor.apply(0) * world.aux[LightData].globalLightStrength(0) )

	def compFogEnd = pov.viewDistance
	fogEnd.set( compFogEnd _ )
	def fogStart = compFogEnd * BaseAnthologiconWorldShader.fogStartPcnt
	oneOverFogRange.set( () => 1.0f / (compFogEnd - fogStart) )

	@Language("GLSL")
	var vertexShaderBody: String =
		"""
			void main () {
				vec4 unpersp = ModelViewMatrix * vec4(Vertex,1.0);
				unpersp.xy += BillboardOffset;
				v_lwc = length(unpersp.xyz);
				v_untransformedZ = Vertex.z + VertexZOffset + 75.0;
		  		gl_Position = ProjectionMatrix * unpersp;
				v_color = Color;
				v_texCoord = TexCoord;
			}
		"""

	@Language("GLSL")
	var fragmentShaderBody: String =
		"""
		   out vec4 o_FragColor;
			void main () {
		  		vec4 baseColor = v_color * texture(precipitationTexture,v_texCoord);

				float fogFactor = clamp((fogEnd - v_lwc) * oneOverFogRange, 0.000001,1.0 );
				vec3 fogColor = mix(lowColor,highColor, clamp(v_untransformedZ * .00909,0.0,1.0) );

				o_FragColor = vec4(mix(fogColor,baseColor.rgb,fogFactor),baseColor.a);
			}
		"""
}

object PrecipitationShader {
	object AttributeProfile extends AttributeProfile(
		"Vertex" -> (3,GL11.GL_FLOAT) ::
		"Color" -> (4,GL11.GL_UNSIGNED_BYTE) ::
		"TexCoord" -> (2,GL11.GL_FLOAT) ::
		"BillboardOffset" -> (2,GL11.GL_FLOAT) ::
		Nil
	) {
		val VertexAttribute = attributesByName("Vertex")
		val TexCoordAttribute = attributesByName("TexCoord")
		val ColorAttribute = attributesByName("Color")
		val BillboardOffsetAttribute = attributesByName("BillboardOffset")

		vertexAttributeIndex = VertexAttribute
		texCoordAttributeIndex = TexCoordAttribute
	}
}
