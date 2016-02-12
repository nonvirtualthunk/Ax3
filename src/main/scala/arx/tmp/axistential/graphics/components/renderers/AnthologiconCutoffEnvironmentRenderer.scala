package arx.axistential.graphics.components.renderers

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 4/24/12
 * Time: 11:41 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.axistential.game.data.world.FluidData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.graphics.helpers.AnthCommonRendering
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.Vec2f
import arx.core.vec.Vec4f
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.TVoxelView
import arx.tmp.game.logic.datastructures.Talea
import arx.tmp.game.logic.world.data.LightData
import arx.graphics.AVBO
import arx.graphics.TextureBlock

class AnthologiconCutoffEnvironmentRenderer extends AnthologiconBaseEnvironmentRenderer with TCutoffEnvironmentRenderer {
	def adjOpen ( x : Int , y : Int , z : Int , window : TVoxelView[Byte] ) = {
		var good = false
		var q = 0; while ( q < 6 && ! good ) {
			if ( q != Cardinals.Bottom && window(x + Cardinals.cardinals(q).x,y + Cardinals.cardinals(q).y,z + Cardinals.cardinals(q).z) <= 0 ) {
				good = true
			}
		q += 1}
		good
	}
	def updateTaleaCutoff(vbo: AVBO, textureBlock: TextureBlock, env: World, wloc: VoxelCoord, wcutoff: Int) {
		val TD = env.aux[TerrainData]
		vbo.attribProfile = attribProfile
		val materials = TD.materialMapping
		val coveringTypes = TD.coveringTypes
		val context = buildDrawingContext(textureBlock,env)
		val texCoords = context.texCoords
		val coveringOffset = materials.size

		var vi = vbo.numPoints
		var ii = vbo.numIndices

		val lightData = env.aux[LightData]

		val localLightData = lightData.localLightingDefinedAt(wloc)
		val nLightChannels = localLightData.size

		val colorHolder = Vec4f(1.0f,1.0f,1.0f,1.0f)
		val texCoordHolder = Vec2f(0.0f,0.0f)

		val talea = TD.materialGrid.windowCenteredOnTaleaAt(VoxelCoord(wloc),readOnly = true)
		if ( ! talea.centerTalea.areAll(0.toByte) ) {
			val centerTalea = talea.centerTalea
			val lightWindow = lightData.globalLighting(0).windowCenteredOnTaleaAt(VoxelCoord(wloc),readOnly = true)
			val coveringWindow = TD.coveringGrid.faceWindowCenteredOnTaleaAt(wloc,readOnly = true)
			val fluidData = env.auxData[FluidData]
			val fluidLevelTalea = fluidData.fluidLevel.taleaForReadOnly(wloc.x,wloc.y,wloc.z)
			val fluidTypeTalea = fluidData.fluidType.taleaForReadOnly(wloc.x,wloc.y,wloc.z)
			val z = (wcutoff) - wloc.z
			val ox = (wloc.x - env.center.x).toFloat
			val oy = (wloc.y - env.center.y).toFloat
			val oz = (wloc.z - env.center.z).toFloat
			val cpTop = Cardinals.cubePoints(Cardinals.Top)
			var x = 0;while ( x < Talea.dimension ) {
				var y = 0;while ( y < Talea.dimension ) {
					var curVoxel = centerTalea(x,y,z)
					if ( curVoxel <= 0 ) {
						val flevel = fluidLevelTalea(x,y,z)
						if ( flevel > fluidData.minFluidCutoff ) {
							curVoxel = fluidTypeTalea(x,y,z)
						}
					}
					val isOpen = adjOpen(x,y,z,talea)
					if ( curVoxel > 0 && ! isOpen ) {
						var h = 1
						while ( y + h < Talea.dimension && talea(x,y + h,z) > 0 && ! adjOpen(x,y+h,z,talea) ) { h += 1 }


						vbo.numPoints += 4
//						vbo.numIndices += 4
						var k = 0; while ( k < 4 ) {
							vbo.setA(V,vi+k,ox + x.toFloat + cpTop(k).x * 1.05f - 0.025f,oy + y.toFloat + cpTop(k).y * h.toFloat,oz + z.toFloat + cpTop(k).z - 0.025f)
							vbo.setAb(LL,vi+k,0.toByte,0.toByte,0.toByte,0.toByte)
							vbo.setAb(GL,vi+k,0.toByte,0.toByte,0.toByte,0.toByte)
							vbo.setA(T,vi+k,0.0f,0.0f)
//							vbo.setI(ii+k,vi+k)
						k += 1}

						vi += 4
//						ii += 4

						y += h
					} else if ( curVoxel > 0 ) {
						val good = true
						var maxGlobalLight = 0.toByte
						var maxLocalLight = 0.0f
						var localLightSum = 0.0f
						colorHolder.r = 0.0f
						colorHolder.g = 0.0f
						colorHolder.b = 0.0f
						var q = 0; while ( q < 6  ) {
							val ax = x + Cardinals.cardinals(q).x
							val ay = y + Cardinals.cardinals(q).y
							val az = z + Cardinals.cardinals(q).z
							maxGlobalLight = math.max( maxGlobalLight, lightWindow(ax,ay,az) ).toByte

							if ( nLightChannels > 0 ) {

								var i = 0;while ( i < nLightChannels ) {
									val localLight = CommonRendering.localLightMults(localLightData(i).window(ax,ay,az))
									maxLocalLight = math.max(maxLocalLight,localLight)
									localLightSum += localLight
									colorHolder.r += localLightData(i).color.r * localLight
									colorHolder.g += localLightData(i).color.g * localLight
									colorHolder.b += localLightData(i).color.b * localLight
								i += 1}
							} else {
								colorHolder.r = 1.0f;colorHolder.g = 1.0f;colorHolder.b = 1.0f
							}
						q += 1}

						if ( localLightSum > 0.0f ) {
							val invLightSum = 1.0f / localLightSum
							colorHolder.r *= invLightSum
							colorHolder.g *= invLightSum
							colorHolder.b *= invLightSum
						}

						if ( good ) {
							val aboveCovering = coveringWindow(x,y,z,Cardinals.Top)
							val matIndex = if ( aboveCovering != 0 ) { aboveCovering + coveringOffset } else { curVoxel }
							val matInfo = context.materialTextureInfo(matIndex)
							val matColor = matInfo.color
							var q = 5; while ( q < 6 ) {
								val cp = Cardinals.cubePoints(q)
								vbo.numPoints += 4
//								vbo.numIndices += 4
								var k = 0; while ( k < 4 ) {
									AnthCommonRendering.materialTexCoords(matInfo,x,y,z,q,k,texCoordHolder)

									vbo.setA(V,vi+k,ox + x.toFloat + cp(k).x,oy + y.toFloat + cp(k).y.toFloat,oz + z.toFloat + cp(k).z - 0.025f)
									vbo.setAbf(LL,vi+k,colorHolder.r,colorHolder.g,colorHolder.b,clamp(maxLocalLight,0.0f,1.9f),128)
//									vbo.setAbf(LL,vi+k,1.0f,1.0f,1.0f,clamp(maxLocalLight,0.0f,1.9f),128)
									vbo.setAbf(GL,vi+k,matColor.r,matColor.g,matColor.b,CommonRendering.lightMults(maxGlobalLight),128)
//									vbo.setAbf(GL,vi+k,1.0f,1.0f,1.0f,1.0f,128)
//									vbo.setA(T,vi+k,/*texCoords(q)(matIndex)(k)*/texCoordHolder)
									vbo.setA(T,vi+k,texCoords(q)(matIndex)(0).x + (texCoordHolder.x) * context.texDims(q)(matIndex),texCoords(q)(matIndex)(0).y + (texCoordHolder.y) * context.texDims(q)(matIndex))
//									vbo.setI(ii+k,vi+k)
								k += 1}
								vi += 4
//								ii += 4
							q += 1}
						}

						y += 1
					} else { y += 1 }
				}
			x += 1}
		}
	}
}
