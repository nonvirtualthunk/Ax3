package arx.axistential.testbed.advancedlighting

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 4/22/14
 * Time: 7:18 AM
 */

import arx.Prelude._
import arx.axistential.game.data.world.TerrainData
import arx.axistential.testbed.TestWorldRendererComponent
import arx.engine.world.World
import arx.tmp.game.logic.datastructures.ByteTalea
import arx.tmp.game.logic.datastructures.GenericTaleaGridWindow
import arx.tmp.game.logic.entities.TLightSource
import arx.tmp.game.logic.world.data.LightData

import scalaxy.loops._

class AdvancedLocalLightRendering extends TestWorldRendererComponent {
	lazy val graphicsInfoProvider = pio[TGameEntityGraphicsInfoProvider]

	lazy val shader = {
		val s = new AdvancedLocalLightShader(world,graphicsEngine.pov)
		s.u_LocalLightPositions.set(() => world.entitiesOfType[TLightSource].map(_.exactLightLocation).toVector)
		s
	}

	override def bucketRequirements =
		RenderBucketRequirements(AdvancedLocalLightAttributeProfile,shader)

	val AP = AdvancedLocalLightAttributeProfile

	override def drawVoxelFace(x: Int, y: Int, z: Int, ox: Float, oy: Float, oz: Float, q: Int,
										terrainWindow: GenericTaleaGridWindow[Byte, ByteTalea],
										lightWindow: GenericTaleaGridWindow[Byte, LightData.LightTaleaType],
										world: World,
										bucket: RenderBucket): Unit =
	{
		val ax = x + cardinalsX(q)
		val ay = y + cardinalsY(q)
		val az = z + cardinalsZ(q)

		val terrain = world.aux[TerrainData]
		val center = terrainWindow.center
		val localLightGrid = world.aux[LightData].lightGridForLightSource(world.entitiesOfType[TLightSource].head)
		val ginfo = graphicsInfoProvider.graphicsInfoFor(terrain.materialForByte(terrainWindow(x,y,z)))
		val texture = ginfo.icon
		val tc = bucket.textureBlock(texture)

		val vi = bucket.incrementVertexOffset(4)
		val ii = bucket.incrementIndexOffset(6)
		val vbo = bucket.vbo

		val lightLevel = CommonRendering.lightMults(lightWindow(ax,ay,az))
		val localLightWindow = localLightGrid.windowCenteredOnTaleaAt(center,readOnly = true)
//		val localLightStrength = CommonRendering.localLightMults(localLightGrid(center.x + ax,center.y + ay,center.z + az))


		val coords = Cardinals.cubePoints(q)
		for ( k <- 0 until 4 optimized ) {
			val oa = Cardinals.cubeOrthos(q)(k)(0)
			val ob = Cardinals.cubeOrthos(q)(k)(1)

//			val llSum =
			val lls = Array (
				localLightWindow(ax,ay,az),
				localLightWindow(ax + oa.x,ay + oa.y,az + oa.z),
				localLightWindow(ax + oa.x + ob.x,ay + oa.y + ob.y,az + oa.z + ob.z),
				localLightWindow(ax + ob.x,ay + ob.y,az + ob.z)
			)
			var llSum = 0.0f
			var llCount = 0.0001f
			for ( ll <- lls ) {
				if ( ll > 0.05f ) {
					llSum += ll
					llCount += 1.0f
				} else {
					llCount += 0.3f
				}
			}
			val ll = CommonRendering.localLightMults((llSum / llCount).toByte)

			vbo.setA(AP.Vertex,vi + k,coords(k).x + ox, coords(k).y + oy, coords(k).z + oz)
			vbo.setAbf(AP.GlobalLightData,vi + k,lightLevel,lightLevel,lightLevel,1.0f,127)
			vbo.setAbf(AP.LocalLightStrengths,vi + k,ll,0.0f,0.0f,0.0f,127)
			vbo.setA(AP.TexCoord,vi + k,tc(k))
			vbo.setAb(AP.SideIndex,vi + k,q.toByte)
		}
		vbo.setIQuad(ii,vi)
	}
}


