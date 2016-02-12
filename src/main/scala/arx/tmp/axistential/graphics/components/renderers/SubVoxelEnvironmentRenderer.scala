package arx.axistential.graphics.components.renderers

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/19/12
 * Time: 2:35 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.axistential.graphics.components.general.AnimationSubVoxel
import arx.core.vec.Vec4f
import arx.engine.world.World
import arx.tmp.game.logic.world.data.LightData
import arx.graphics.TextureBlock
import arx.graphics.traits.TRenderTarget

import scala.collection.GenTraversable

class SubVoxelEnvironmentRenderer extends TSubVoxelEnvironmentRenderer with AnthologiconBaseEnvironmentRenderer {
	val subVoxelResolutionShift = 2
	val subVoxelResolution = 1 << subVoxelResolutionShift
	val invSubVoxelResolution = 1.0f / subVoxelResolution.toFloat

	val subCubePoints = Cardinals.cubePoints.map( _.map( _ * invSubVoxelResolution ) )


	def renderSubVoxels(renderTarget: TRenderTarget, textureBlock: TextureBlock, env: World, subVoxels: GenTraversable[AnimationSubVoxel]) {
		if ( subVoxels.isEmpty ) { return }
		val lightData = env.aux[LightData]

		val context = buildDrawingContext(textureBlock,env)
		val presumedCenter = subVoxels.head.coord
		val localLightData = lightData.allLocalLightChannels
		val colorHolder = Vec4f(0.0f,0.0f,0.0f,0.0f)
		val nLightChannels = localLightData.size
		val vbo = renderTarget.vbo
		vbo.attribProfile = attribProfile

		val globalLightGrid = lightData.globalLighting(0)

		for ( subVoxel <- subVoxels ) {
			val v = subVoxel.coord; val wx = v.x; val wy = v.y; val wz = v.z
			val subX = v.subX * invSubVoxelResolution
			val subY = v.subY * invSubVoxelResolution
			val subZ = v.subZ * invSubVoxelResolution
			val ox = wx.toFloat - env.centerf.x;val oy = wy.toFloat - env.centerf.y;val oz = wz.toFloat - env.centerf.z
			val curVoxel = subVoxel.value

			if ( curVoxel > 0 ) {
				var q = 0
				while ( q < 6 ) {
					val lx = wx + cardinalsX(q)
					val ly = wy + cardinalsY(q)
					val lz = wz + cardinalsZ(q)
					val maxGlobalLight = globalLightGrid(lx,ly,lz)
					var maxLocalLight = 0.0f
					var localLightSum = 0.0f
					colorHolder.r = 0.0f
					colorHolder.g = 0.0f
					colorHolder.b = 0.0f
					if ( nLightChannels > 0 ) {
						var i = 0;while ( i < nLightChannels ) {
							val localLight = CommonRendering.localLightMults(localLightData(i).grid(lx,ly,lz))
							maxLocalLight = math.max(maxLocalLight,localLight)
							localLightSum += localLight
							colorHolder.r += localLightData(i).color.r * localLight
							colorHolder.g += localLightData(i).color.g * localLight
							colorHolder.b += localLightData(i).color.b * localLight
						i += 1}
					}

					if ( localLightSum > 0.0f ) {
						val invLightSum = 1.0f / localLightSum
						colorHolder.r *= invLightSum
						colorHolder.g *= invLightSum
						colorHolder.b *= invLightSum
					}

					val voxelIndex = curVoxel
					val materialInfo = context.materialTextureInfo(voxelIndex)
					val sidingMult = context.sidingMults(q)

					val tcstart = context.texCoords(q)(voxelIndex)(0)
					val tcDim = context.texCoords(q)(voxelIndex)(2).x - tcstart.x

					val vi = renderTarget.incrementVertexOffset(4)
					val ii = renderTarget.incrementIndexOffset(4)

					var k = 0;while ( k < 4 ) {

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
							val nx = if ( q == Left || q == Right ) { subZ + subCubePoints(q)(k).z } else { subX + subCubePoints(q)(k).x }
							val ny = if ( q == Front || q == Back ) { subZ + subCubePoints(q)(k).z } else { subY + subCubePoints(q)(k).y }
							val bx = (mx & materialInfo.spreadAND).toFloat * materialInfo.spreadMult + nx * materialInfo.spreadMult //0.0 0.25 0.5 0.75 0.0
							val by = (my & materialInfo.spreadAND).toFloat * materialInfo.spreadMult + ny * materialInfo.spreadMult
							tx = bx
							ty = by
						}



						val cp = subCubePoints(q)(k)
						vbo.setA(V,vi+k,cp.x+subX+ox,cp.y+subY+oy,cp.z+subZ+oz)
						vbo.setAbf(LL,vi+k,colorHolder.r,colorHolder.g,colorHolder.b,maxLocalLight * sidingMult,128)
						vbo.setAbf(GL,vi+k,materialInfo.color.r,materialInfo.color.g,materialInfo.color.b,CommonRendering.lightMults(maxGlobalLight) * sidingMult,128)
						vbo.setA(T,vi+k,tcstart.x + (tx) * tcDim,tcstart.y + (ty) * tcDim)
						vbo.setI(ii+k,vi+k)
						k += 1
					}
				q += 1}
			}
		}
	}
}