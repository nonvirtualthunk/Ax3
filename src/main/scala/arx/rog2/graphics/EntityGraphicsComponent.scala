package arx.rog2.graphics

/**
  * TODO: Add javadoc
  */

import arx.application.Noto
import arx.core.math.Rectf
import arx.core.vec.Cardinals
import arx.core.vec.Cardinals
import arx.core.vec.Cardinals
import arx.core.vec.Cardinals
import arx.core.vec.Vec2f
import arx.core.vec.Vec3f
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.graphics.GraphicsEngine
import arx.graphics.helpers.HSBA
import arx.graphics.pov.TopDownCamera
import arx.resource.ResourceManager
import arx.rog2.game.data.entity.Creature
import arx.rog2.game.data.entity.CubeDrawInfo
import arx.rog2.game.data.entity.DrawHeight
import arx.rog2.game.data.entity.LightSource
import arx.rog2.game.data.entity.Physical
import arx.rog2.game.data.entity.SliceDrawInfo
import arx.rog2.game.data.entity.TextureDrawInfo
import arx.rog2.game.data.world.Terrain

class EntityGraphicsComponent(engine: GraphicsEngine) extends RogGraphicsComponent(engine) {

	override def draw(canvas: RogCanvas): Unit = {
		val ctx = new LightAndVisionContext
		val terrain = world[Terrain]

		val playerPos = player[Physical].position
		val visionLimit = player[Creature].sightRange
		for (ent <- world.auxDataQuery[Physical].results) {
			val PD = ent[Physical]
			if (PD.heldIn.isEmpty) {
				val dim = PD.dimensions.inVoxels
				if (PD.position.distanceTo(playerPos) < visionLimit + PD.dimensions.x + PD.dimensions.y + PD.dimensions.z) {
					val color = if (ent.hasAuxData[Creature]) {
						val CD = ent[Creature]

						PD.color.mix(HSBA(0.0f, 1.0f, 0.9f, 1.0f), CD.damageTaken / CD.maxHP.toFloat)
					} else {
						HSBA(0.0f, 0.0f, 1.0f, 1.0f)
					}


					for (arX <- 0 until dim.x.toInt; arY <- 0 until dim.y.toInt) {
						val subPos = PD.position + Vec3i(arX, arY, 0)
						val light = if (ent.hasAuxData[LightSource]) {
							new LightHolder(1.0f, ent[LightSource].lightColor)
						}
						else {
							ctx.lightPcntAt(subPos, Cardinals.Center)
						}
						val lightPcnt = light.lightPcnt

						val vision = ctx.visionPcntAt(subPos, Cardinals.Center)
						if (vision > 0.5f) {
							PD.drawInfo match {
								case TextureDrawInfo(texture, drawHeight) =>
									val z = drawHeight match {
										case DrawHeight.Center => 0.0f
										case DrawHeight.Floor => -0.45f
									}

									canvas.quad(subPos - VoxelCoord.Center + Vec3f(0.0f, 0.0f, z + PD.drawOrder * 0.01f))
										.withColor(color * HSBA(1.0f, 1.0f, lightPcnt, 1.0f))
										.withVisionPcnt(vision)
										.withLightColor(light.lightColor)
										.withDimensions(1.0f, 1.0f)
										.withSubTexture(ResourceManager.image(s"rog/entities/$texture.png"), Rectf(arX / dim.x.floor, arY / dim.y.floor, 1 / dim.x.floor, 1 / dim.y.floor))
										.draw()
								case SliceDrawInfo(texture, rot) =>
									val img = ResourceManager.image(s"rog/entities/$texture.png")
									val sections = img.height / img.width
									val secHeight = dim.z / sections
									for (i <- 0 until sections) {
										val eps = 0.001f
										canvas.quad(subPos - VoxelCoord.Center + Vec3f(0.0f, 0.0f, -0.4f + secHeight * i))
											.withColor(color * HSBA(1.0f, 1.0f, lightPcnt, 1.0f))
											.withVisionPcnt(vision)
											.withLightColor(light.lightColor)
											.withDimensions(1.0f, 1.0f)
											.withSubTexture(img, Rectf(0.0f + arX / dim.x.floor + eps, (i + arY / dim.y.floor) / sections.toFloat + eps, 1.0f / dim.y.floor - eps*2.0f, (1.0f / sections) / dim.y.floor - eps*2.0f))
											.withTextureIndexRotation(rot)
											.draw()
									}
								case _ =>
							}
						}
					}


					PD.drawInfo match {
						case CubeDrawInfo(texture) =>
							for (arX <- 0 until dim.x.toInt; arY <- 0 until dim.y.toInt; arZ <- 0 until dim.z.toInt; q <- 0 until 6) {
								val subPos = PD.position + Vec3i(arX, arY, arZ)

								val vision = ctx.visionPcntAt(subPos, q)
								if (vision > 0.0f) {
									val c = Cardinals.dirvec(q)
									val adjX = subPos.x + c.x
									val adjY = subPos.y + c.y
									val adjZ = subPos.z + c.z
									if (terrain.voxel(adjX, adjY, adjZ).isSentinel) {
										val light = if (ent.hasAuxData[LightSource]) {
											new LightHolder(1.0f, ent[LightSource].lightColor)
										}
										else {
											ctx.lightPcntAt(subPos, q)
										}
										val lightPcnt = light.lightPcnt

										val (aTexX,aDimX) = q match {
											case Cardinals.Front => (dim.x.toInt - arX - 1) -> dim.x
											case Cardinals.Back => arX -> dim.x
											case Cardinals.Left => (dim.y.toInt - arY - 1) -> dim.y
											case Cardinals.Right => arY -> dim.y
											case Cardinals.Top | Cardinals.Bottom => arX -> dim.x
										}
										val (aTexY,aDimY) = if (c.z != 0) { arY -> dim.y } else { arZ -> dim.z }

										canvas.quad(subPos - VoxelCoord.Center + Vec3f(0.0f, 0.0f, PD.drawOrder * 0.01f))
											.withColor(color * HSBA(1.0f, 1.0f, lightPcnt, 1.0f))
											.withVisionPcnt(vision)
											.withLightColor(light.lightColor)
											.withDimensions(1.0f, 1.0f)
											.withSubTexture(ResourceManager.image(s"rog/entities/$texture.png"), Rectf(aTexX / aDimX.floor, aTexY / aDimY.floor, 1 / aDimX.floor, 1 / aDimX.floor))
											.withCubeFace(q)
											.draw()
									}
								}

							}
						case _ => //do nothing
					}
				}
			}
		}

		val PD = player[Physical]
		pov.resolve() match {
			case tdc: TopDownCamera =>
				tdc.moveSpeed = Vec3f(0.0f, 0.0f, 0.0f)
				tdc.turnSpeed = Vec2f(0.0f)
				tdc.eye = Vec3f(PD.position.x - VoxelCoord.Center.x, PD.position.y - VoxelCoord.Center.y, tdc.eye.z)
			case _ =>
		}

		Noto.info("Creature graphics component updated")
	}
}
