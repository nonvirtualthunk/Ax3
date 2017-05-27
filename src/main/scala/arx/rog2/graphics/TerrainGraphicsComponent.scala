package arx.rog2.graphics

/**
  * TODO: Add javadoc
  */

import arx.Prelude
import arx.application.Noto
import arx.core.vec.Cardinals
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec3f
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.DrawPriority
import arx.engine.simple.Canvas
import arx.graphics.helpers.HSBA
import arx.resource.ResourceManager
import arx.rog2.game.data.entity.Creature
import arx.rog2.game.data.entity.LightSource
import arx.rog2.game.data.entity.Physical
import arx.rog2.game.data.world.Light
import arx.rog2.game.data.world.RogData
import arx.rog2.game.data.world.Terrain
import arx.rog2.game.data.world.TerrainFlag
import arx.rog2.game.entities.Material

import scalaxy.loops._

class TerrainGraphicsComponent(engine: GraphicsEngine) extends RogGraphicsComponent(engine) {
	drawOrder = DrawPriority.Early

	val cardinals2d = Array(ReadVec3i(-1, 0, 0), ReadVec3i(0, 1, 0), ReadVec3i(1, 0, 0), ReadVec3i(0, -1, 0))
	val allCardinals = Cardinals.cardinals

	val borderImages = Array(0,1,3,4).map(i => i -> ResourceManager.image(s"rog/materials/border_$i.png")).toMap

	override def draw(canvas: RogCanvas): Unit = {
		val TD = world[Terrain]
		val RD = world[RogData]

		val playerPosition = player[Physical].position
		val playerSightRange = player[Creature].sightRange.inVoxels.toInt

		val ctx = new LightAndVisionContext

		for (dx <- -playerSightRange to playerSightRange optimized; dy <- -playerSightRange to playerSightRange optimized) {

			for (z <- VoxelCoord.Center.z -8 to VoxelCoord.Center.z + 8) {
				val v = VoxelCoord(playerPosition.x + dx, playerPosition.y + dy, z)
				val rv = v - VoxelCoord.Center
				val vox = TD.voxel(v)
				val mat: Material = vox.material

				if (mat.notSentinel && !vox.hasFlag(TerrainFlag.Ceiling)) {
					val img = ResourceManager.image(s"rog/materials/${mat.texture}.png")

					var maxSight = 0.0f
					var drawnSides = 0
					for (q <- 0 until 6; c = allCardinals(q)) {
						val sideMult = if (c.z == 0) { 0.85f } else if (c.z < 0) { 0.7f } else { 1.0f }

						val adj = v + c
						if (TD.voxel(adj).isSentinel) {
							drawnSides |= (1 << q)
							val visionPcnt = ctx.visionPcntAt(v, q)
							val light = if (visionPcnt > 0.5f) {
								ctx.lightPcntAt(v, q)
							} else {
								ctx.noLight
							}
							val lightPcnt = light.lightPcnt

							maxSight = maxSight.max(visionPcnt)

							val color = mat.color * HSBA(1.0f,Prelude.sqrtf(lightPcnt),sideMult * lightPcnt,1.0f)

							// standard side-of-voxel display
							canvas.quad(rv)
								.withColor(color)
								.withTexture(img)
								.withDimensions(1.0f, 1.0f)
								.withVisionPcnt(visionPcnt)
								.withLightColor(light.lightColor)
								.withCubeFace(q)
								.draw()

							// outlining where wall meets floor
							if (c.z == 0 && TD.voxel(adj + Vec3i(0, 0, -1)).notSentinel) {
								canvas.quad(Vec3f(rv) + (Vec3f(c) * 0.55f) - Vec3f(0.0f, 0.0f, 0.45f))
									.withColor(HSBA(0.2f, 0.0f, 0.2f * lightPcnt, 1.0f))
									.withTexture(ResourceManager.blankImage)
									.withDimensions((1.0f - c.x.abs).max(0.1f), (1.0f - c.y.abs).max(0.1f))
									.withVisionPcnt(visionPcnt)
									.withLightColor(1.0f,1.0f,1.0f)
									.draw()
							}
						} else if (TD.voxel(adj).hasFlag(TerrainFlag.Ceiling)) {
							val color = mat.color * HSBA(1.0f,0.8f,0.5f,1.0f)

							// draw ceiling cutoff level
							canvas.quad(rv)
								.withColor(color)
								.withTexture(img)
								.withDimensions(1.0f, 1.0f)
								.withVisionPcnt(maxSight)
								.withLightColor(1.0f,1.0f,1.0f)
								.withCubeFace(q)
								.draw()

							// draw ceiling outline
							for (q <- 0 until 5 optimized) {
								if (q != Cardinals.Bottom && (drawnSides & (1 << q)) != 0) {
									canvas.quad(Vec3f(rv) + Vec3f(0.0f,0.0f,0.01f * (q+1)))
										.withColor(HSBA(1.0f,0.0f,0.0f,1.0f))
										.withTexture(borderImages(q))
										.withDimensions(1.0f, 1.0f)
										.withVisionPcnt(1.0f)
										.withLightColor(1.0f,1.0f,1.0f)
										.withCubeFace(Cardinals.Top)
										.draw()
								}
							}
						}
					}
				}
			}
		}
	}
}
