package arx.axistential.graphics.components.renderers

import arx.application.Noto
import arx.axistential.game.data.world.GranularData
import arx.axistential.game.data.world.TerrainData
import arx.core.vec.Vec4f
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec3i
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.DummyInfiniteByteVoxelStore
import arx.tmp.game.logic.datastructures.Talea
import arx.tmp.game.logic.world.data.LightData
import arx.graphics.AVBO
import arx.graphics.AttributeProfile
import arx.graphics.TextureBlock
import org.lwjgl.opengl.GL11._

import scala.collection.mutable
import scalaxy.loops._

/**
 */

class AnthologiconEnvironmentRenderer extends EnvironmentRenderer with AnthologiconBaseEnvironmentRenderer {
	val lastSizeByLocation = new mutable.HashMap[Vec3i,(Int,Int)]
//	def attributeProfile = attribProfile

	val useShorts = true

	val prof = new AttributeProfile(
		"vertex" -> (4,if ( useShorts ) { GL_SHORT } else { GL_FLOAT }) ::
		"texCoords" -> (2,GL_UNSIGNED_SHORT) ::
		"localLight" -> (4,GL_UNSIGNED_BYTE) ::
		"globalLight" -> (4,GL_UNSIGNED_BYTE) ::
		Nil
	)
	override val V = attribProfile.attributesByName("vertex")
	prof.attributes(V).normalize = false
	override val LL = attribProfile.attributesByName("localLight")
	override val GL = attribProfile.attributesByName("globalLight")
	override val T = attribProfile.attributesByName("texCoords")

	def attributeProfile = prof

	var noSpread = false
	def updateTalea ( vbo: AVBO , textureBlock : TextureBlock , env : World , wloc: VoxelCoord , offset : ReadVec3i , limit : ReadVec3i ){
		Analytics.timeDataPoint("anthologicon environment renderer") {
			try{
				if ( vbo.writingActive ) { Noto.severeError("Two rendering processes operating on same vbo simultaneously"); return }
				vbo.writingActive = true
				vbo.attribProfile = attributeProfile

				val context = buildDrawingContext(textureBlock,env)
				val texCoords = context.texCoords
				val coveringOffset = context.coveringOffset
				val sidingMults = context.sidingMults
				val cardinals = Cardinals.cardinals
				val cubeOrthos = Cardinals.cubeOrthos
				val cubePointsFlat = Cardinals.cubePointsFlat
				val cubePointsFlati = Cardinals.cubePointsFlati
				val cubePointsi = Cardinals.cubePointsi
				val Left = Cardinals.Left
				val Right = Cardinals.Right
				val Front = Cardinals.Front
				val Back = Cardinals.Back
				val Top = Cardinals.Top
				val Bottom = Cardinals.Bottom

				val terrainData = env.auxData[TerrainData]
				val terrain = terrainData._materialGrid
				val coverings = terrainData._coveringGrid

				var v = vbo.numPoints
				var i = vbo.numIndices

				val worldOffset = Vec3i(wloc - env.center)
				val ox = worldOffset.x
				val oy = worldOffset.y
				val oz = worldOffset.z

				val lightMult : Array[Float] = new Array[Float](CommonRendering._lightMults.length)
				Array.copy(CommonRendering._lightMults,0,lightMult,0,CommonRendering._lightMults.length)
//				val localLightMults = CommonRendering._localLightMults

				val window = terrain.windowCenteredOnTaleaAt(wloc,readOnly = true)

				val coveringWindow = coverings.faceWindowCenteredOnTaleaAt(wloc,readOnly = true)
				if ( ! window.centerTalea.areAll(0.toByte) ) {
					vbo.numPointsHint( 100 )
					vbo.numIndicesHint( 100 )

					val lightData = env.auxData[LightData]
					val lightWindow = lightData.globalLighting(0).windowCenteredOnTaleaAt(wloc,readOnly = true)
					val localLightData = lightData.localLightingDefinedAt(wloc)

					val granularData = env.auxData[GranularData]
					val granularLevelWindow = granularData.levelGrid.windowCenteredOnTaleaAt(wloc,readOnly = true)

					val terrainCenter = window.centerTalea
					val lightCenter = lightWindow.centerTalea

					val nLightChannels = localLightData.size
					val avgLocalLight = Array.ofDim[Float](nLightChannels)



					val colorHolder = Vec4f(0.0f,0.0f,0.0f,1.0f)
					for (x <- offset.x until limit.x optimized ; y <- offset.y until limit.y optimized ; z <- offset.z until limit.z optimized ) {
						val curVoxel = terrainCenter(x,y,z)
						if ( curVoxel > 0 ){
							val useCenter = x >= 1 && x < Talea.dimension -1 && y >= 1 && y < Talea.dimension -1  && z >= 1 && z < Talea.dimension - 1
							val lightView = if ( useCenter ) { lightCenter } else { lightWindow }
							val terrainView = if ( useCenter ) { terrainCenter } else { window }

							for (q <- 0 until 6 optimized) {
								val ax = x + cardinals(q).x
								val ay = y + cardinals(q).y
								val az = z + cardinals(q).z
								val adj = terrainView(ax,ay,az)

								if ( adj <= 0 && (q != Top || granularLevelWindow(ax,ay,az) == 0)){
									val coveringForSide = coveringWindow(x,y,z,q)
									val voxelIndex = if ( coveringForSide != 0 ) { coveringOffset + coveringForSide } else { curVoxel }
									val materialInfo = context.materialTextureInfo(voxelIndex)

									val tcstart = texCoords(q)(voxelIndex)(0)
									val tcDim = texCoords(q)(voxelIndex)(2).x - tcstart.x
									val dontSpread = noSpread || (!materialInfo.spread)//|| (! (texCoords(q)(voxelIndex) eq texCoords(Cardinals.Top)(voxelIndex)))

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
													val rawLightByte = lightView(lx,ly,lz)
													globalLightStrength += lightMult(math.max(rawLightByte,0))
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

//											globalLightStrength = lightMult(math.max(lightView(ax,ay,az),0))

											if ( nLightChannels > 0 ) {
												colorHolder.r = 0.0f; colorHolder.g = 0.0f; colorHolder.b = 0.0f
												var lightSum = 0.000001f

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

												if ( lightSum > localLightStrength && localLightStrength < 0.85f ) {
													localLightStrength = math.min(0.85f,localLightStrength + (lightSum - localLightStrength) * 0.75f)
												}
												colorHolder.r /= lightSum
												colorHolder.g /= lightSum
												colorHolder.b /= lightSum
//															colorHolder.r = 1.0f; colorHolder.g = 1.0f; colorHolder.b = 1.0f
											} else { //if we don't have locals, the light color will just be global light's color
												colorHolder.r = 1.0f; colorHolder.g = 1.0f; colorHolder.b = 1.0f
											}
										} else {
											colorHolder.r = 0.0f; colorHolder.g = 0.0f; colorHolder.b = 0.0f
										}

										for (lli <- 0 until nLightChannels optimized) {
											avgLocalLight(lli) = 0.0f
										}

										val cp = cubePointsFlat((q << 2) + k)

										var tx = 0.0f
										var ty = 0.0f
										if ( dontSpread ) {
											if ( k == 1 || k == 2 ) { tx = 1.0f }
											if ( k == 2 || k == 3 ) { ty = 1.0f }
										} else if ( materialMirror ) {
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
											val mx = if ( q == Front || q == Back ) { x } else { y }//if ( q == Left || q == Right ) { z } else { x }
											val my = if ( q == Top || q == Bottom ) { x } else { z }//if ( q == Front || q == Back ) { z } else { y }
											val nx = if ( q == Front || q == Back ) { cubePointsi(q)(k).x } else { cubePointsi(q)(k).y }//if ( q == Left || q == Right ) { cubePointsi(q)(k).z } else { cubePointsi(q)(k).x }
											val ny = if ( q == Top || q == Bottom ) { cubePointsi(q)(k).x } else { cubePointsi(q)(k).z } //if ( q == Front || q == Back ) { cubePointsi(q)(k).z } else { cubePointsi(q)(k).y }
											val bx = (mx & materialInfo.spreadAND).toFloat * materialInfo.spreadMult + nx * materialInfo.spreadMult //0.0 0.25 0.5 0.75 0.0
											val by = (my & materialInfo.spreadAND).toFloat * materialInfo.spreadMult + ny * materialInfo.spreadMult
											tx = bx
											ty = by
										}


										if ( useShorts ) {
											val cpi = cubePointsFlati((q << 2) + k)
											vbo.setAs(V,v + k,(cpi.x + x + ox).toShort,(cpi.y + y + oy).toShort,(cpi.z + z + oz).toShort)
										} else {
											vbo.setA(V,v + k,cp.x + x + ox,cp.y + y + oy,cp.z + z + oz)
										}

										vbo.setAbf(LL,v + k, colorHolder.r,colorHolder.g,colorHolder.b,localLightStrength * sidingMult,128)
										vbo.setAbf(GL,v + k, materialColor.r,materialColor.g,materialColor.b,globalLightStrength * sidingMult,128)
//													vbo.setA(T,v + k,tcstart.x + (tx) * tcDim,tcstart.y + (ty) * tcDim)
										vbo.setAs(T,v + k,((tcstart.x + tx * tcDim) * 65535).toShort,((tcstart.y + ty * tcDim) * 65535).toShort)

										k += 1
									}

									vbo.setIQuad(i,v)

									v += 4
									i += 6
								}
							}
						}
					}
				}

//				lastSizeByLocation.put(wloc,(vbo.numPoints,vbo.numIndices))
				vbo.writingActive = false
			} catch {
				case err : OutOfMemoryError =>
					Noto.error("Failed to allocate sufficient direct memory while updating talea, clearing VBO and continuing")
					vbo.clear()
					vbo.unsolidify()
			}
		}
	}

	override def updateVoxels(vbo: AVBO, textureBlock: TextureBlock, env: World, locs: IndexedSeq[ReadVec3i], locOffset: VoxelCoord , settings : VoxelRenderingSettings ) {
		if ( vbo.writingActive ) { throw new IllegalStateException("Multiple threads writing to vbo at same time") }
		vbo.writingActive = true

		if ( locs.nonEmpty ) {
			vbo.attribProfile = attribProfile

//			val facesOn = settings.facesEnabled match {
//				case None => (1<<0)|(1<<1)|(1<<2)|(1<<3)|(1<<4)|(1<<5)
//				case Some(arr) => var s = 0;for ( i <- 0 until 6 ) { if ( arr(i) ) { s |= (1 << i) } }; s
//			}

			val presumedCenter = locOffset

			val terrainData = env.aux[TerrainData]
			val lightData = env.aux[LightData]

			val terrainWindow = terrainData._materialGrid.windowCenteredOnTaleaAt(presumedCenter,true)
			val lightWindow = lightData.globalLighting(0).windowCenteredOnTaleaAt(presumedCenter,true)
			val coveringWindow = terrainData._coveringGrid.faceWindowCenteredOnTaleaAt(presumedCenter,true)
			val context = buildDrawingContext(textureBlock,env)
			val fullLight = lightData.FullLight
			var i = 0;
			var vi = vbo.numPoints
			var ii = vbo.numIndices
			var altVoxStore = settings.alternateBlockSource.getOrElse(DummyInfiniteByteVoxelStore)
			val cubePointsi = Cardinals.cubePointsi

			val localLightData = lightData.localLightingDefinedAt(presumedCenter)
			val nLightChannels = localLightData.size

			val colorHolder = Vec4f(1.0f,1.0f,1.0f,1.0f)
			val centerf = env.centerf

			while ( i < locs.length ) {
				val v = locs(i); val wx = v.x + locOffset.x; val wy = v.y + locOffset.y; val wz = v.z + locOffset.z
				val ox = wx.toFloat - centerf.x;val oy = wy.toFloat - centerf.y;val oz = wz.toFloat - centerf.z
				val lx = wx - terrainWindow.center.x;val ly = wy - terrainWindow.center.y;val lz = wz - terrainWindow.center.z
				val curVoxel = terrainWindow(lx,ly,lz)

				if ( curVoxel > 0 ) {
					var maxGlobalLight = 0.toByte
					var maxLocalLight = 0.0f
					var localLightSum = 0.0f
					colorHolder.r = 0.0f
					colorHolder.g = 0.0f
					colorHolder.b = 0.0f
					for (q <- 0 until 6 optimized) {
						val ax = lx + Cardinals.cardinals(q).x;val ay = ly + Cardinals.cardinals(q).y;val az = lz + Cardinals.cardinals(q).z
						maxGlobalLight = math.max(maxGlobalLight,lightWindow(ax,ay,az)).toByte
						if ( nLightChannels > 0 ) {
							var i = 0;while ( i < nLightChannels ) {
								val localLight = CommonRendering.localLightMults(localLightData(i).window(ax,ay,az))
								maxLocalLight = math.max(maxLocalLight,localLight)
								localLightSum += localLight
								colorHolder.r += localLightData(i).color.r * localLight
								colorHolder.g += localLightData(i).color.g * localLight
								colorHolder.b += localLightData(i).color.b * localLight
								i += 1}
						}
					}
					if ( localLightSum > 0.0f ) {
						val invLightSum = 1.0f / localLightSum
						colorHolder.r *= invLightSum
						colorHolder.g *= invLightSum
						colorHolder.b *= invLightSum
					}

					for (q <- 0 until 6 optimized) {
//						if ( (facesOn & q) != 0 ) {
							val ax = lx + Cardinals.cardinals(q).x;val ay = ly + Cardinals.cardinals(q).y;val az = lz + Cardinals.cardinals(q).z
							val adjVoxel = terrainWindow(ax,ay,az)
							val adjVoxel2 = altVoxStore(ax,ay,az)
							if ( (adjVoxel <= 0 || adjVoxel2 <= 0) || ! settings.exposedFacesOnly ) {
								val coveringForSide = coveringWindow(lx,ly,lz,q)
								val voxelIndex = if ( coveringForSide != 0 ) { context.coveringOffset + coveringForSide } else { curVoxel }
								val materialInfo = context.materialTextureInfo(voxelIndex)

								val sidingMult = context.sidingMults(q)

								val tcstart = context.texCoords(q)(voxelIndex)(0)
								val tcDim = context.texCoords(q)(voxelIndex)(2).x - tcstart.x

								vbo.numPoints = vi + 4;
								if ( settings.useIndices ) { vbo.numIndices = ii + 4; }
								var k = 0;while ( k < 4 ) {

									var tx = 0.0f
									var ty = 0.0f
									if ( ! materialInfo.spread ) {
										if ( k == 1 || k == 2 ) { tx = 1.0f }
										if ( k == 2 || k == 3 ) { ty = 1.0f }
									} else if ( materialInfo.mirror ) {
										val mx = if ( q == Cardinals.Left || q == Cardinals.Right ) { wz+cubePointsi(q)(k).z } else { wx+cubePointsi(q)(k).x }
										val my = if ( q == Cardinals.Front || q == Cardinals.Back ) { wz+cubePointsi(q)(k).z } else { wy+cubePointsi(q)(k).y }
										val bx = ((mx) & materialInfo.spreadAND).toFloat * materialInfo.spreadMult//0.0 0.25 0.5 0.75 0.0
										val by = ((my) & materialInfo.spreadAND).toFloat * materialInfo.spreadMult
										val cx = ((mx >> materialInfo.spreadShift) & 1) //0 0 0 0 1 1 1 1
										val cy = ((my >> materialInfo.spreadShift) & 1)
										val dx = cx * -2 + 1 //1 1 1 1 -1 -1 -1 -1
										val dy = cy * -2 + 1
										tx = cx.toFloat + bx * dx.toFloat
										ty = cy.toFloat + by * dy.toFloat
									} else {
										val mx = if ( q == Cardinals.Left || q == Cardinals.Right ) { wz } else { wx }
										val my = if ( q == Cardinals.Front || q == Cardinals.Back ) { wz } else { wy }
										val nx = if ( q == Cardinals.Left || q == Cardinals.Right ) { cubePointsi(q)(k).z } else { cubePointsi(q)(k).x }
										val ny = if ( q == Cardinals.Front || q == Cardinals.Back ) { cubePointsi(q)(k).z } else { cubePointsi(q)(k).y }
										val bx = (mx & materialInfo.spreadAND).toFloat * materialInfo.spreadMult + nx * materialInfo.spreadMult //0.0 0.25 0.5 0.75 0.0
										val by = (my & materialInfo.spreadAND).toFloat * materialInfo.spreadMult + ny * materialInfo.spreadMult
										tx = bx
										ty = by
									}



									val cp = Cardinals.cubePoints(q)(k)
									vbo.setA(V,vi+k,cp.x+ox,cp.y+oy,cp.z+oz)
									vbo.setAbf(LL,vi+k,colorHolder.r,colorHolder.g,colorHolder.b,maxLocalLight * sidingMult,128)
									vbo.setAbf(GL,vi+k,materialInfo.color.r,materialInfo.color.g,materialInfo.color.b,CommonRendering.lightMults(maxGlobalLight) * sidingMult,128)
	//								vbo.setAb(C,vi+k,
	//									(materialInfo.color.r * effectiveLight * sidingMult * 255).toByte,
	//									(materialInfo.color.g * effectiveLight * sidingMult * 255).toByte,
	//									(materialInfo.color.b * effectiveLight * sidingMult * 255).toByte,
	//									(materialInfo.color.a * settings.opacity * 255).toByte
	//								)
	//								vbo.setT(vi+k,context.texCoords(q)(voxelIndex)(k))
									vbo.setA(T,vi+k,tcstart.x + (tx) * tcDim,tcstart.y + (ty) * tcDim)
									if ( settings.useIndices ) { vbo.setIQuad(ii+k,vi+k) }
								k += 1}
								vi += 4
								ii += 4
							}
//						}
					}
				}
			i += 1}
		}
		vbo.writingActive = false
	}
}