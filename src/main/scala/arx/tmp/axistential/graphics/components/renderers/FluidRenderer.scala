package arx.axistential.graphics.components.renderers

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/1/12
 * Time: 9:28 AM
 * Created by nonvirtualthunk
 */

import arx.application.Noto
import arx.axistential.game.data.world.FluidData
import arx.axistential.game.data.world.TerrainData
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec3i
import arx.core.vec.Vec3f
import arx.core.vec.Vec4f
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.Talea
import arx.tmp.game.logic.world.data.LightData
import arx.graphics.AVBO
import arx.graphics.TextureBlock

class FluidRenderer extends AnthologiconBaseEnvironmentRenderer with TEnvironmentRenderer {


	def attributeProfile = StandardAttributeProfile

	def effectiveFluidLevel(fl: Short) = if ( fl < 0 ) { (fl & 0x7fff).toShort } else { fl }

	def updateTalea ( vbo: AVBO , textureBlock : TextureBlock , env : World ,wloc: VoxelCoord , offset : ReadVec3i , limit : ReadVec3i ){
		Analytics.timeDataPoint("liquid renderer") {
		try{
			if ( vbo.writingActive ) { Noto.severeError("Two rendering processes operating on same vbo simultaneously"); return }
			val liquidData = env.auxData[FluidData]
			if ( ! liquidData.definedAt(wloc) || liquidData.fluidLevel.taleaForReadOnly(wloc.x,wloc.y,wloc.z).areAll(0.toByte) ) { return }

			vbo.writingActive = true
			val V = StandardAttributeProfile.VertexAttribute
			val GL = StandardAttributeProfile.GlobalLightAttribute
			val LL = StandardAttributeProfile.LocalLightAttribute
			val T = StandardAttributeProfile.TexCoordAttribute

			val context = buildDrawingContext(textureBlock,env)
			val texCoords = context.texCoords
			val sidingMults = context.sidingMults

			val TD = env.aux[TerrainData]
			val terrain = TD.materialGrid

			var v = vbo.numPoints
			var i = vbo.numIndices

			val worldOffset = Vec3f(wloc - env.center)
			val ox = worldOffset.x
			val oy = worldOffset.y
			val oz = worldOffset.z


			val liquidTypeWindow = liquidData.fluidType.windowCenteredOnTaleaAt(wloc,readOnly = true)
			val liquidLevelWindow = liquidData.fluidLevel.windowCenteredOnTaleaAt(wloc,readOnly = true)
			val terrainWindow = terrain.windowCenteredOnTaleaAt(wloc,readOnly = true)


			vbo.numPointsHint( 100 )
			vbo.numIndicesHint( 100 )

			val lightData = env.aux[LightData]
			val lightWindow = lightData.globalLighting(0).windowCenteredOnTaleaAt(wloc,readOnly = true)
			val localLightData = lightData.localLightingDefinedAt(wloc)

			val liquidLevelCenter = liquidLevelWindow.centerTalea
			val liquidTypeCenter = liquidTypeWindow.centerTalea

			val lightCenter = lightWindow.centerTalea
			val terrainCenter = terrainWindow.centerTalea

			val nLightChannels = localLightData.size
			val avgLocalLight = Array.ofDim[Float](nLightChannels)

			val oneOverMaxFluidLevel = (1.0f / liquidData.maxFluidLevel.toFloat) * 0.9f
			val minimumUsableFluidLevel = 0
			val minimumUsableFluidHeight = oneOverMaxFluidLevel * minimumUsableFluidLevel

			val fluidHeightsArray = Array(0.0f,0.0f,0.0f,0.0f)

			val liquidCheckOffsets = Array[Vec3i]( Vec3i(0,0,0) , Vec3i(1,0,0) , Vec3i(1,1,0) , Vec3i(0,1,0) )

			val colorHolder = Vec4f(0.0f,0.0f,0.0f,1.0f)
			var x = offset.x
			while ( x < limit.x ) {
				var y = offset.y
				while ( y < limit.y ) {
					var z = offset.z
					while ( z < limit.z ){
						val curLevel = effectiveFluidLevel( liquidLevelCenter(x,y,z) )
						if ( curLevel > 0 ){
							val useCenter = x >= 1 && x < Talea.dimension -1 && y >= 1 && y < Talea.dimension -1  && z >= 1 && z < Talea.dimension - 1
							val lightView = if ( useCenter ) { lightCenter } else { lightWindow }
							val fluidLevelView = if ( useCenter ) { liquidLevelCenter } else { liquidLevelWindow }
							val liquidTypeView = if ( useCenter ) { liquidTypeCenter } else { liquidTypeWindow }
							val terrainView = if ( useCenter ) { terrainCenter } else { terrainWindow }
							var fluidHeights : Array[Float] = null
							var maximumFluidHeight = 0.0f
							var minimumFluidHeight = 1.0f

							var q = 0
							while ( q < 6 ){
								val ax = x + cardinals(q).x
								val ay = y + cardinals(q).y
								val az = z + cardinals(q).z
								val adjLevel = effectiveFluidLevel( fluidLevelView(ax,ay,az) )

								if ( adjLevel <= minimumUsableFluidLevel+2 ){
									if ( fluidHeights == null ) {
										fluidHeights = fluidHeightsArray

										var fi = 0; while ( fi < 4 ) {
											fluidHeights(fi) = 0.0f
											var counter = 0.000001f
											var fj = 0; while ( fj < 4 ) {
												val fx = x - 1 + liquidCheckOffsets(fj).x + liquidCheckOffsets(fi).x
												val fy = y - 1 + liquidCheckOffsets(fj).y + liquidCheckOffsets(fi).y
												val fz = z

												if ( effectiveFluidLevel( fluidLevelView(fx,fy,fz + 1) ) > minimumUsableFluidLevel ) {
													fluidHeights(fi) = 100000.0f
												} else {
													val fl = effectiveFluidLevel( fluidLevelView(fx,fy,fz) )
													if ( fl > 0 ) {
														fluidHeights(fi) += fl * oneOverMaxFluidLevel
														counter += 1.0f
													}
												}
											fj += 1}
											fluidHeights(fi) = math.min(fluidHeights(fi) / counter,1.0f)
											maximumFluidHeight = math.max(fluidHeights(fi),maximumFluidHeight)
											minimumFluidHeight = math.min(fluidHeights(fi),minimumFluidHeight)
										fi += 1}
									}

									if ( maximumFluidHeight > minimumUsableFluidHeight && minimumFluidHeight > minimumUsableFluidHeight) {
										val liquidTypeIndex = liquidTypeView(x,y,z)
										val materialInfo = context.materialTextureInfo(liquidTypeIndex)

										val tcstart = texCoords(q)(liquidTypeIndex)(0)
										val tcDim = texCoords(q)(liquidTypeIndex)(2).x - tcstart.x

										vbo.numPoints = v + 4
										vbo.numIndices = i + 6

										val sidingMult: Float = sidingMults(q)
										val materialColor: Vec4f = materialInfo.color
										val materialMirror: Boolean = materialInfo.mirror
										var k = 0
										while ( k < 4 ){
											var globalLightStrength = 0.0f

											var counter = 0.0f
											var iaxis = 0x0
											while ( iaxis >= -1 ) {
												var jaxis = 0x0
												while ( jaxis >= -1 ) {
													val lx = ax + (cubeOrthos(q)(k)(0).x & iaxis) + (cubeOrthos(q)(k)(1).x & jaxis)
													val ly = ay + (cubeOrthos(q)(k)(0).y & iaxis) + (cubeOrthos(q)(k)(1).y & jaxis)
													val lz = az + (cubeOrthos(q)(k)(0).z & iaxis) + (cubeOrthos(q)(k)(1).z & jaxis)



													if ( terrainView(lx,ly,lz) <= 0.toByte ) {
														globalLightStrength += CommonRendering.lightMults(lightView(lx,ly,lz))
														var lli = 0
														while ( lli < nLightChannels ) {
															avgLocalLight(lli) += CommonRendering.localLightMults(localLightData(lli).window(lx,ly,lz))
														lli += 1}
														counter += 1.0f
													} else {
														counter += 0.3f
													}
												jaxis += 0xffffffff }
											iaxis += 0xffffffff }

											var localLightStrength = 0.0f
											if ( counter > 0.0f ) {
												globalLightStrength /= counter

												if ( nLightChannels > 0 ) {
													colorHolder.r = 0.0f; colorHolder.g = 0.0f; colorHolder.b = 0.0f
													var lightSum = 0.0f

													var lli = 0
													while ( lli < nLightChannels ) {
														avgLocalLight(lli) /= counter
														localLightStrength = math.max(localLightStrength,avgLocalLight(lli))
														lightSum += avgLocalLight(lli)
														colorHolder.r += localLightData(lli).color.r * avgLocalLight(lli)
														colorHolder.g += localLightData(lli).color.g * avgLocalLight(lli)
														colorHolder.b += localLightData(lli).color.b * avgLocalLight(lli)
														lli += 1
													}

													colorHolder.r /= lightSum
													colorHolder.g /= lightSum
													colorHolder.b /= lightSum
												} else { //if we don't have locals, the light color will just be global light's color
													colorHolder.r = 1.0f; colorHolder.g = 1.0f; colorHolder.b = 1.0f
												}
											} else {
												colorHolder.r = 0.0f; colorHolder.g = 0.0f; colorHolder.b = 0.0f
											}

											var lli = 0
											while ( lli < nLightChannels ) { avgLocalLight(lli) = 0.0f; lli += 1 }


											val cp = cubePointsFlat((q << 2) + k)

											var tx = 0.0f
											var ty = 0.0f

											if ( materialMirror ) {
												val mx = if ( q == Left || q == Right ) { z+cubePointsi(q)(k).z } else { x+cubePointsi(q)(k).x }
												val my = if ( q == Front || q == Back ) { z+cubePointsi(q)(k).z } else { y+cubePointsi(q)(k).y }
												val bx = ((mx) & materialInfo.spreadAND).toFloat * materialInfo.spreadMult//0.0 0.25 0.5 0.75 0.0
												val by = ((my) & materialInfo.spreadAND).toFloat * materialInfo.spreadMult
												val cx = ((mx >> materialInfo.spreadShift) & 1) //0 0 0 0 1 1 1 1
												val cy = ((my >> materialInfo.spreadShift) & 1)
												val dx = cx * -2 + 1 //1 1 1 1 -1 -1 -1 -1
												val dy = cy * -2 + 1
												tx = cx.toFloat + bx * dx.toFloat
												ty = cy.toFloat + by * dy.toFloat
											} else {
												val mx = if ( q == Left || q == Right ) { z } else { x }
												val my = if ( q == Front || q == Back ) { z } else { y }
												val nx = if ( q == Left || q == Right ) { cubePointsi(q)(k).z } else { cubePointsi(q)(k).x }
												val ny = if ( q == Front || q == Back ) { cubePointsi(q)(k).z } else { cubePointsi(q)(k).y }
												val bx = (mx & materialInfo.spreadAND).toFloat * materialInfo.spreadMult + nx * materialInfo.spreadMult //0.0 0.25 0.5 0.75 0.0
												val by = (my & materialInfo.spreadAND).toFloat * materialInfo.spreadMult + ny * materialInfo.spreadMult
												tx = bx
												ty = by
											}

											val fluidZ = if ( cp.x < 0.5f && cp.y < 0.5f ) {
												fluidHeights(0)
											} else if ( cp.x > 0.5f && cp.y < 0.5f ) {
												fluidHeights(1)
											} else if ( cp.x > 0.5f && cp.y > 0.5f ) {
												fluidHeights(2)
											} else {
												fluidHeights(3)
											}
											vbo.setA(V,v + k,cp.x + x + ox,cp.y + y + oy,cp.z * fluidZ + z + oz)
											vbo.setAbf(LL,v + k, colorHolder.r,colorHolder.g,colorHolder.b,localLightStrength * sidingMult,128)
											vbo.setAbf(GL,v + k, materialColor.r,materialColor.g,materialColor.b,globalLightStrength * sidingMult,128)
											vbo.setA(T,v + k,tcstart.x + (tx) * tcDim,tcstart.y + (ty) * tcDim)

											k += 1
										}
										vbo.setIQuad(i,v)

										v += 4
										i += 6
									}
								}
								q += 1
							}
						}
						z += 1
					}
					y += 1
				}
				x += 1
			}

			vbo.writingActive = false
		} catch {
			case err : OutOfMemoryError =>
				Noto.error("Failed to allocate sufficient direct memory while updating talea, clearing VBO and continuing")
				vbo.clear()
				vbo.unsolidify()
		}
	}
	}
}

