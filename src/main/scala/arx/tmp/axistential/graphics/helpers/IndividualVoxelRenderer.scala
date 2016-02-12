package arx.axistential.graphics.helpers

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/31/14
 * Time: 8:28 AM
 */

import arx.application.Noto
import arx.axistential.game.archetypes.Material
import arx.axistential.game.data.world.TerrainData
import arx.axistential.graphics.components.renderers.EnvironmentDrawingContext
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.MergedInfiniteVoxelView
import arx.tmp.game.logic.datastructures.TInfiniteVoxelView
import arx.tmp.game.logic.world.data.LightData
import arx.graphics.TextureBlock
import arx.graphics.traits.TRenderTarget

import scalaxy.loops._

class IndividualVoxelRenderer(textureBlock : TextureBlock, world : World) {
	private final val TD = world.aux[TerrainData]
	private final val LD = world.aux[LightData]
	private final val lighter = new VoxelLighter(world)
	protected def drawingContext = EnvironmentDrawingContext.fromWorld(world,textureBlock)
	/** If set, only draw those faces that are exposed, uses the exposureGrid to determine that */
	var exposedOnly = true
	/** grid to use to determine exposed-ness and presence of this voxel to be drawn, if absent TerrainData.materialGrid will be used */
	var exposureGrid : Option[TInfiniteVoxelView[Byte]] = None

	def withExposedOnly (eo : Boolean) = { exposedOnly = eo; this }
//	def withTransforms (t : Transforms) = { transforms = t; this }
	def withExposureGrid (grid : Option[TInfiniteVoxelView[Byte]]) = { exposureGrid = grid ; this }
	def withExposureGrids (grids : TInfiniteVoxelView[Byte] *) = {
		if (grids.length > 1) {
			exposureGrid = Some(new MergedInfiniteVoxelView[Byte](grids.toArray,0.toByte))
			this
		} else {
			withExposureGrid(grids.headOption)
		}
	}

	/**
	 * Render an individual voxel
 *
	 * @param vox voxel coordinate to draw
	 * @param material the material to draw for this voxel
	 */
	def drawVoxel (renderTarget : TRenderTarget, vox : VoxelCoord , material : Material , transforms : Transforms = Transforms.Identity) {
		val ctxt = drawingContext
		val matIndex = TD._materialMapping(material)
		val grid = exposureGrid.getOrElse(TD.materialGrid)
		val materialInfo = ctxt.materialTextureInfo(matIndex)
		val wx = vox.x; val wy = vox.y; val wz = vox.z
		val oc = vox.toObjectCoord


		for (q <- 5 to 0 by -1 optimized) {
			val texCoords = ctxt.texCoords(q)(matIndex)
			val tcstart = texCoords(0)
			val tcDim =texCoords(2).x - tcstart.x
			// Removed "if (grid(vox) > 0), seemed kinda redundant since we're explicitly telling it to draw
			if ( (! exposedOnly || grid(vox + Cardinals.cardinals(q)) <= 0) ) {
				val vi = renderTarget.incrementVertexOffset(4)
				val ii = renderTarget.incrementIndexOffset(6)

				var k =0; while ( k < 4 ) {
					val lighting = lighter.lightOnVoxelPoint(vox,q,k)
					var tx = 0.0f
					var ty = 0.0f
					if ( ! materialInfo.spread ) {
						if ( k == 1 || k == 2 ) { tx = 1.0f }
						if ( k == 2 || k == 3 ) { ty = 1.0f }
					} else if ( materialInfo.mirror ) {
						val mx = if ( q == Left || q == Right ) { wz+Cardinals.cubePointsi(q)(k).z } else { wx+Cardinals.cubePointsi(q)(k).x }
						val my = if ( q == Front || q == Back ) { wz+Cardinals.cubePointsi(q)(k).z } else { wy+Cardinals.cubePointsi(q)(k).y }
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
						val nx = if ( q == Left || q == Right ) { Cardinals.cubePointsi(q)(k).z * transforms.zPercent } else { Cardinals.cubePointsi(q)(k).x }
						val ny = if ( q == Front || q == Back ) { Cardinals.cubePointsi(q)(k).z * transforms.zPercent } else { Cardinals.cubePointsi(q)(k).y }
						val bx = (mx & materialInfo.spreadAND).toFloat * materialInfo.spreadMult + nx * materialInfo.spreadMult //0.0 0.25 0.5 0.75 0.0
						val by = (my & materialInfo.spreadAND).toFloat * materialInfo.spreadMult + ny * materialInfo.spreadMult
						tx = bx
						ty = by
					}

					if ( (renderTarget.vbo.attribProfile eq UIAttributeProfile) || (renderTarget.vbo.attribProfile eq BillboardAttributeProfile) ) {
						val globalStrength = LD.globalLightStrength(0) * lighting.globalLight

						val ox = oc.x + Cardinals.centeredCubePoints(q)(k).x * transforms.dimensionMultiplier.x + transforms.dimensionAdder.x
						val oy = oc.y + Cardinals.centeredCubePoints(q)(k).y * transforms.dimensionMultiplier.y + transforms.dimensionAdder.y
						val oz = oc.z - 0.5f + (Cardinals.centeredCubePoints(q)(k).z + 0.5f) * transforms.zPercent * transforms.dimensionMultiplier.z + transforms.dimensionAdder.z
						if ( renderTarget.vbo.attribProfile eq UIAttributeProfile ) {
							val pcntLocal = lighting.localLightStrength / math.max(globalStrength + lighting.localLightStrength,0.0001f)
							val blendedColor = arx.MathPrelude.mix(LD.globalLightColor(0),lighting.localLightColor,pcntLocal)
							val effStrength = math.max(globalStrength,lighting.localLightStrength)
							val color = blendedColor * effStrength * materialInfo.color.rgb * transforms.colorMultiplier.rgb

							renderTarget.vbo.setAbf(UIAttributeProfile.ColorAttribute,vi+k,color.r,color.g,color.b,materialInfo.color.a * transforms.colorMultiplier.a,128)
							renderTarget.vbo.setA(UIAttributeProfile.VertexAttribute,vi+k,ox,oy,oz)
							renderTarget.vbo.setA(UIAttributeProfile.TexCoordAttribute,vi+k,tcstart.x + (tx) * tcDim,tcstart.y + (ty) * tcDim)
							renderTarget.vbo.setA(UIAttributeProfile.BillboardOffsetAttribute,vi+k,0.0f,0.0f,0.0f)
						} else if ( renderTarget.vbo.attribProfile eq BillboardAttributeProfile ) {
							val color = materialInfo.color
							val llColor = lighting.localLightColor
							renderTarget.vbo.setAbf(BillboardAttributeProfile.GlobalLightAttribute,vi+k,color.r,color.g,color.b,globalStrength * ctxt.sidingMults(q),128)
							renderTarget.vbo.setAbf(BillboardAttributeProfile.LocalLightAttribute,vi+k,llColor.r,llColor.g,llColor.b,lighting.localLightStrength * ctxt.sidingMults(q),128)
							renderTarget.vbo.setA(BillboardAttributeProfile.VertexAttribute,vi+k,ox,oy,oz)
							renderTarget.vbo.setA(BillboardAttributeProfile.TexCoordAttribute,vi+k,tcstart.x + (tx) * tcDim,tcstart.y + (ty) * tcDim)
							renderTarget.vbo.setA(BillboardAttributeProfile.BillboardOffsetAttribute,vi+k,0.0f,0.0f)
						}
					} else { arx.Prelude.doOnce(()=>Noto.warn("Invalid attrib profile type in common rendering draw individual voxel")) }

					k += 1}
				renderTarget.vbo.setIQuad(ii,vi)
			}
		}
	}

}
