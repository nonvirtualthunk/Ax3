package arx.axistential.graphics.helpers

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/9/13
 * Time: 2:22 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.application.Noto
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.graphics.graphicsinfo.MaterialGraphicsInfo
import arx.core.vec._
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.GridBuffer
import arx.tmp.game.logic.datastructures.TInfiniteVoxelView
import arx.tmp.game.logic.world.data.LightData
import arx.graphics.traits.TRenderTarget

object AnthCommonRendering {
	def materialTexCoords ( materialInfo : MaterialGraphicsInfo , x : Int, y : Int , z : Int , q: Int , k : Int , res : Vec2f ) {
		var tx = 0.0f
		var ty = 0.0f
		if ( ! materialInfo.spread ) {
			if ( k == 1 || k == 2 ) { tx = 1.0f }
			if ( k == 2 || k == 3 ) { ty = 1.0f }
		} else if ( materialInfo.mirror ) {
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
		res.x = tx
		res.y = ty
	}

	def lightOnObject( env : World, t : TPhysicalEntity ) = {
		val lightData = env.aux[LightData]
		val TD = env.aux[TerrainData]
		val terrain = TD.materialGrid
		val baseDim = t.fastOrientedDimensions.inVoxels
		val dim = Vec3i(baseDim)
		var lightSum = 0.0f
		var maxGlobLight = 0.0f
		var maxLocLight = 0.0f

		val tmpPos = Vec3f(0,0,0)
		var i = 0; while ( i < 3 ) {
			if ( arx.MathPrelude.absf(baseDim(i)) < 1.0f ) { tmpPos(i) = t.position(i) } else { tmpPos(i) = t.position(i) - baseDim(i) * 0.5f }
			i += 1}

		val pos = ObjectCoord(tmpPos).toVoxelCoord
		val ox = pos.x;val oy = pos.y;val oz = pos.z
		val colorHolder = Vec3f(0.0f,0.0f,0.0f)

		val centerTerrainMat = TD.materialForByte(terrain(t.position.toVoxelCoord))
		val useEdges = centerTerrainMat.notSentinel
		val transparency = centerTerrainMat.byteTransparency
		val useCenter = transparency <= 0
		val boostCenter = -transparency.min(0)

		var startX = 0; var startY = 0; var startZ = 0
		var endX = dim.x; var endY = dim.y; var endZ = dim.z
		if ( useEdges ) {
			startX = -1; startY = -1; startZ = -1
			endX += 1; endY += 1; endZ += 1
		}

		val globalLightBuffer = GridBuffer(lightData.globalLighting(0))
		val localLightChannels = lightData.allLocalLightChannels
		val localLightBuffers = Array.ofDim[GridBuffer[Byte,LightData.LightTaleaType]](localLightChannels.length)
		var li = 0; while ( li < localLightChannels.length ) {
			localLightBuffers(li) = GridBuffer(localLightChannels(li).grid)
			li += 1}
		var x = startX; while ( x <= endX ) {
			var y = startY; while ( y <= endY ) {
				var z = startZ; while ( z <= endZ ) {
					val center = (! useEdges || (x >= 0 && x <= dim.x && y >= 0 && y <= dim.y && z >= 0 && z < dim.z))
					if ( useCenter || ! center ) {
						val glv = globalLightBuffer(ox+x,oy+y,oz+z) + (if (center) { boostCenter } else { 0 })
						if ( glv >= 0 ) {
							val g = CommonRendering.lightMults( glv )
							maxGlobLight = math.max(maxGlobLight,g)


							var xli = 0;while ( xli < localLightChannels.length ){
								val l = CommonRendering.localLightMults( localLightBuffers(xli)(ox+x,oy+y,oz+z) )
								val localLightColor = localLightChannels(xli).color.resolve()
								maxLocLight = math.max(maxLocLight,l)
								lightSum += l
								colorHolder.r += l * localLightColor.r
								colorHolder.g += l * localLightColor.g
								colorHolder.b += l * localLightColor.b
								xli += 1}
						}
					}
					z += 1}
				y += 1}
			x += 1}

		if ( lightSum > 0.0001f ) {
			colorHolder.r /= lightSum
			colorHolder.g /= lightSum
			colorHolder.b /= lightSum
		} else {
			colorHolder.r = 1.0f
			colorHolder.g = 1.0f
			colorHolder.b = 1.0f
		}

		LightResult( maxGlobLight , maxLocLight , colorHolder )
	}


	/**
	 * Render an individual voxel
 *
	 * @param bucket target to render to
	 * @param vox voxel coordinate to draw
	 * @param grid grid to use to determine exposed-ness and presence of this voxel to be drawn, if absent will be drawn regardless
	 * @param color color to draw
	 * @param materialInfo info about texture spread and mirroring
	 * @param texCoords texture coordinates to apply
	 * @param exposedOnly only draw those faces that are exposed, will only be used if grid is non empty
	 */
	def drawIndividualVoxel ( bucket : TRenderTarget , vox : VoxelCoord , grid : Option[TInfiniteVoxelView[Byte]] , color : Vec4f , materialInfo : MaterialGraphicsInfo, texCoords : Array[ReadVec2f] , exposedOnly : Boolean ) {
		val wx = vox.x; val wy = vox.y; val wz = vox.z
		var q = 5; while ( q >= 0 ) {
			val tcstart = texCoords(0)
			val tcDim =texCoords(2).x - tcstart.x
			if ( grid.isEmpty || (grid.get(vox) > 0 && (! exposedOnly || grid.get(vox + Cardinals.cardinals(q)) <= 0)) ) {
				val vi = bucket.incrementVertexOffset(4)
				val ii = bucket.incrementIndexOffset(6)

				var k =0; while ( k < 4 ) {

					var tx = 0.0f
					var ty = 0.0f
					if ( ! materialInfo.spread ) {
						if ( k == 1 || k == 2 ) { tx = 1.0f }
						if ( k == 2 || k == 3 ) { ty = 1.0f }
					} else if ( materialInfo.mirror ) {
						val mx = if ( q == Left || q == Right ) { wz+cubePointsi(q)(k).z } else { wx+cubePointsi(q)(k).x }
						val my = if ( q == Front || q == Back ) { wz+cubePointsi(q)(k).z } else { wy+cubePointsi(q)(k).y }
						val bx = ((mx) & materialInfo.spreadAND).toFloat * materialInfo.spreadMult//0.0 0.25 0.5 0.75 0.0
						val by = ((my) & materialInfo.spreadAND).toFloat * materialInfo.spreadMult
						val cx = ((mx >> materialInfo.spreadShift) & 1) //0 0 0 0 1 1 1 1
						val cy = ((my >> materialInfo.spreadShift) & 1)
						val dx = cx * -2 + 1 //1 1 1 1 -1 -1 -1 -1
						val dy = cy * -2 + 1
						tx = cx.toFloat + bx * dx.toFloat
						ty = cy.toFloat + by * dy.toFloat
					} else {
						val mx = if ( q == Left || q == Right ) { wz } else { wx }
						val my = if ( q == Front || q == Back ) { wz } else { wy }
						val nx = if ( q == Left || q == Right ) { cubePointsi(q)(k).z } else { cubePointsi(q)(k).x }
						val ny = if ( q == Front || q == Back ) { cubePointsi(q)(k).z } else { cubePointsi(q)(k).y }
						val bx = (mx & materialInfo.spreadAND).toFloat * materialInfo.spreadMult + nx * materialInfo.spreadMult //0.0 0.25 0.5 0.75 0.0
						val by = (my & materialInfo.spreadAND).toFloat * materialInfo.spreadMult + ny * materialInfo.spreadMult
						tx = bx
						ty = by
					}

					if ( bucket.vbo.attribProfile eq UIAttributeProfile ) {
						bucket.vbo.setAbf(UIAttributeProfile.ColorAttribute,vi+k,color.r,color.g,color.b,color.a,128)
						bucket.vbo.setA(UIAttributeProfile.VertexAttribute,vi+k,vox.toObjectCoord + Cardinals.centeredCubePoints(q)(k))
						bucket.vbo.setA(UIAttributeProfile.TexCoordAttribute,vi+k,tcstart.x + (tx) * tcDim,tcstart.y + (ty) * tcDim)
						bucket.vbo.setA(UIAttributeProfile.BillboardOffsetAttribute,vi+k,0.0f,0.0f,0.0f)
					} else if ( bucket.vbo.attribProfile eq BillboardAttributeProfile ) {
						arx.Prelude.doOnce(()=>Noto.warn("drawIndividualVoxel is not fully set up to work with BillboardAttributeProfile"))
						bucket.vbo.setAbf(BillboardAttributeProfile.GlobalLightAttribute,vi+k,color.r,color.g,color.b,1.0f,128)
						bucket.vbo.setAbf(BillboardAttributeProfile.LocalLightAttribute,vi+k,1.0f,1.0f,1.0f,0.0f,128)
						bucket.vbo.setA(BillboardAttributeProfile.VertexAttribute,vi+k,vox.toObjectCoord + Cardinals.centeredCubePoints(q)(k))
						bucket.vbo.setA(BillboardAttributeProfile.TexCoordAttribute,vi+k,tcstart.x + (tx) * tcDim,tcstart.y + (ty) * tcDim)
						bucket.vbo.setA(BillboardAttributeProfile.BillboardOffsetAttribute,vi+k,0.0f,0.0f)
					} else { arx.Prelude.doOnce(()=>Noto.warn("Invalid attrib profile type in common rendering draw individual voxel")) }

					k += 1}
				bucket.vbo.setIQuad(ii,vi)
			}
			q -= 1}
	}

}
