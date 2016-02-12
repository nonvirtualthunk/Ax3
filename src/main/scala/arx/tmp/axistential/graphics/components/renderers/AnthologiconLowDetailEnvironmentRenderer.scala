package arx.axistential.graphics.components.renderers

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 4/21/12
 * Time: 2:24 PM
 * Created by nonvirtualthunk
 */

import arx.axistential.game.data.world.TerrainData
import arx.core.vec.ReadVec3i
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.ByteTalea
import arx.tmp.game.logic.datastructures.FaceTaleaGridWindow
import arx.tmp.game.logic.datastructures.GenericTaleaGridWindow
import arx.tmp.game.logic.datastructures.Talea
import arx.tmp.game.logic.world.data.LightData
import arx.graphics._

class AnthologiconLowDetailEnvironmentRenderer extends AnthologiconBaseEnvironmentRenderer with TLowDetailEnvironmentRenderer {
	def attributeProfile = attribProfile


	def updateTalea(vbo: AVBO, textureBlock: TextureBlock, env: World, wloc: VoxelCoord, offset: ReadVec3i, limit: ReadVec3i) {
		updateTaleaLowDetail(vbo,textureBlock,env,wloc,offset,limit)
	}

	def sliceIndex(x : Int,y : Int, q: Int) = (q + (x << 3) + (y << (Talea.dimensionPo2+3)) )
	def fillOutSlice(z: Int, coveringOffset : Byte,voxes: Array[Byte], lights: Array[Float], terrainWindow: GenericTaleaGridWindow[Byte, ByteTalea], coveringWindow: FaceTaleaGridWindow, lightWindow: GenericTaleaGridWindow[Byte, LightData.LightTaleaType]) {
//		val coveringsCenter = coveringWindow.centerTalea
		val terrainCenter = terrainWindow.centerTalea
		val lightCenter = lightWindow.centerTalea
		val centerableZ = z >= 1 && z < Talea.dimension - 1
		var x = 0;while ( x < Talea.dimension ) {
			var y = 0;while ( y < Talea.dimension ) {
				val useCenter = centerableZ && (x >= 1 && x < Talea.dimension - 1 && y >= 1 && y < Talea.dimension - 1)
				val terrainView = if ( useCenter ) { terrainCenter } else { terrainWindow }
				val lightView = if ( useCenter ) { lightCenter } else { lightWindow }

				val currentVoxel = terrainCenter(x,y,z)
//				val combinedCoverings = if ( currentVoxel > 0 ) { coveringsCenter(x,y,z) } else { 0 }
				var q = 0;while ( q < 6 ) {
					val idx = sliceIndex(x,y,q)
					if ( currentVoxel > 0 ) {
						val adj = terrainView(x + cardinals(q).x,y + cardinals(q).y,z + cardinals(q).z)
						if ( adj <= 0 ) {
							val currentCovering = coveringWindow(x,y,z,q)
							if ( currentCovering != 0 ) {
								voxes(idx) = (currentCovering + coveringOffset).toByte
							} else {
								voxes(idx) = currentVoxel
							}
							lights(idx) = CommonRendering.lightMults(lightView(x + cardinals(q).x,y + cardinals(q).y,z + cardinals(q).z))
						} else { voxes(idx) = 0.toByte;lights(idx) = 0.0f }
					} else { voxes(idx) = 0.toByte;lights(idx) = 1.0f }
				q += 1}
			y += 1}
		x += 1}
	}

	def updateTaleaLowDetail(vbo: AVBO, textureBlock: TextureBlock, env: World, wloc: VoxelCoord, offset: ReadVec3i, limit: ReadVec3i) {
		Analytics.timeDataPoint("anthologicon low detail environment renderer") {
			vbo.attribProfile = attribProfile
			if ( limit.x < Talea.dimension ) { throw new IllegalArgumentException(this.getClass + " cannot currently handle sub talea updates") }
			vbo.writingActive = true

			vbo.numPoints = 0
			vbo.numIndices = 0
//			vbo.activeArrays = VBO.Vertex | VBO.TexCoord | VBO.Color

			val TD = env.aux[TerrainData]
			val lightData = env.aux[LightData]

			val terrainWindow = TD.materialGrid.windowCenteredOnTaleaAt(wloc,true)
			if ( ! terrainWindow.centerTalea.areAll(0.toByte) ) {
				val voxes = new Array[Byte](Talea.dimension * Talea.dimension * 8)
				val lights = new Array[Float](Talea.dimension * Talea.dimension * 8)


				val lightWindow = lightData.globalLighting(0).windowCenteredOnTaleaAt(wloc,true)
				val coveringWindow = TD.coveringGrid.faceWindowCenteredOnTaleaAt(wloc,true)
				val coveringTypes = TD.coveringTypes
				val materialTypes = TD.materialMapping
				val coveringOffset = materialTypes.size.toByte
				val context = buildDrawingContext(textureBlock,env)
				val texCoords = context.texCoords

				val worldOffset = (wloc - env.center)
				val ox = worldOffset.x.toFloat;val oy = worldOffset.y.toFloat;val oz = worldOffset.z.toFloat


				vbo.numPointsHint(100)
				vbo.numIndicesHint(100)


				var vi = 0;var ii = 0;
				var z = offset.z;while ( z < limit.z ) {
					fillOutSlice(z,coveringOffset,voxes,lights,terrainWindow,coveringWindow,lightWindow)
					var q = 0;while ( q < 6 ) {
						val sidingMult = q match {
							case Cardinals.Top => 0.98f
							case Cardinals.Bottom => 0.89f
							case Cardinals.Left | Cardinals.Right => 0.93f
							case _ => 0.9f
						}
						val cp = cubePoints(q)

						var x = 0;while ( x < limit.x ) {
							var y = 0;while ( y < limit.y ) {
								val idx = sliceIndex(x,y,q)
								var curVox = voxes(idx)
								var h = 1;
								if ( curVox > 0 ) {
									val light = lights(idx)
									val materialColor = context.materialTextureInfo(curVox).color


									vbo.numPoints = vi + 4
									vbo.numIndices = ii + 4

									var w = 1;

									if ( q != Cardinals.Left && q != Cardinals.Right ) {
										while ( w < 12 && w + x < limit.x &&
											(//(light < 0.01f && lights( sliceIndex(x + w,y) ) < 0.01f) ||
												voxes( sliceIndex(x + w,y,q) ) == curVox && math.abs(lights( sliceIndex(x + w,y,q) ) - light) < 0.02f) )
										{
											voxes(sliceIndex(x + w,y,q)) = 0.toByte
											w += 1
										}
									}
									if ( q != Cardinals.Front && q != Cardinals.Back ) {
										var continue = true;
										while ( h < 12 && h + y < limit.y && continue ) {
											var tx = 0; while ( tx < w && continue ) {
//												if ( ! (light < 0.01f && lights(sliceIndex(x+tx,y+h)) < 0.01f) ) {
												val nidx = sliceIndex(x + tx,y + h,q)
													if ( voxes(nidx) != curVox || math.abs(lights( nidx ) - light) >= 0.02f ) { continue = false; }
//												}
											tx += 1}
											if ( continue ) {
												var tx = 0; while ( tx < w ) { voxes(sliceIndex(x + tx,y + h,q)) = 0.toByte; tx += 1 }
												h += 1
											}
										}
									}

									var k = 0;while ( k < 4 ) {
										vbo.setA(V,vi+k,x.toFloat + ox + cp(k).x * w.toFloat,y.toFloat + oy + cp(k).y * h.toFloat,z.toFloat + oz + cp(k).z)
//										vbo.setAb(C,vi+k,
//											(light * materialColor.r * sidingMult * 255).toByte,
//											(light * materialColor.g * sidingMult * 255).toByte,
//											(light * materialColor.b * sidingMult * 255).toByte,
//											255.toByte
//										)
										vbo.setA(T,vi+k,texCoords(q)(voxes(idx))(k))
										vbo.setI(ii+k,vi+k)
									k += 1}

									vi += 4
									ii += 4
								}
							y += h}
						x += 1}
					q += 1}
				z += 1}
			}
			vbo.writingActive = false
		}
	}
}