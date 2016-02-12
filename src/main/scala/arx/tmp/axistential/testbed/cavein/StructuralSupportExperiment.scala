package arx.axistential.testbed.cavein

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/22/14
 * Time: 8:15 AM
 */

import arx.Prelude._
import arx.axistential.game.archetypes.CommonMaterials
import arx.axistential.game.archetypes.Material
import arx.axistential.game.components.construction.StructuralSupportGameComponent
import arx.axistential.game.data.world.StructuralSupportData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.logic.intersection.WorldIntersector
import arx.axistential.game.logic.structuralsupport.TStructuralSupportLogic
import arx.axistential.graphics.shader.GameUIShader
import arx.core.units.UnitOfTime
import arx.core.vec._
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.DummyInfiniteByteVoxelStore
import arx.resource.ResourceManager

import scalaxy.loops._

class StructuralSupportExperiment extends BareBonesTestbed {
	lazy val ssl = pio[TStructuralSupportLogic]
	val structComp = new StructuralSupportGameComponent

	var activeMaterial = CommonMaterials.Stone

	override def setUpGameEngine(): Unit = {
		val TD = gameEngine.world.aux[TerrainData]
		TD._setFromFunction((x,y,z) => {
			if (z == VoxelCoord.Center.z) {
				if (x < VoxelCoord.Center.x) {
					CommonMaterials.Stone
				} else {
					Material.withName("soil")
				}
			}
			else { Material.Sentinel }
		},15)
		TD.materialGrid.enableLogging()

		gameEngine addComponent structComp
	}

	override def setUpUI(): Unit = {
		gameController.addTopLevelMode(new TGameMode {
			onEvent {
				case WorldMousePressEvent(button,start,end,modifiers) => {
					WorldIntersector.intersect(world,start,end,DummyInfiniteByteVoxelStore,(r) => true) match {
						case Some(WorldIntersectionResult(voxel,_,side)) => {
							if (modifiers.alt) {
								import StructuralSupportData._
								val SSD = world.aux[StructuralSupportData]
								val src = StructuralSupportData.unpackSource(world.aux[StructuralSupportData].supportGrid(voxel))
								println("Support level : " + StructuralSupportData.unpackSupport(world.aux[StructuralSupportData].supportGrid(voxel)) + " from : " +
									src.direction + " arch(" + src.isInArch + "), keystone(" + src.isKeystone
									+ "), dirty(" + ((src & StructuralSupportData.DirtyBit) != 0) + ")" )
							} else if (modifiers.ctrl) {
								val v = voxel
								world.aux[TerrainData].materialGrid.modificationBlock(v) {
									world.aux[TerrainData].setMaterialAt(v,Material.Sentinel)
//									world.aux[StructuralSupportData].supportGrid(v) = StructuralSupportData.PackedUninitialized
								}
							} else {
								val baseV = voxel + dirvec(side)
								val mdz = if (modifiers.shift) { 10 } else { 1 }
								for ( z <- 0 until mdz optimized ) {
									val v = baseV + dirvec(side) * z
									world.aux[TerrainData].materialGrid.modificationBlock(v) {
										world.aux[TerrainData].setMaterialAt(v,activeMaterial)
										world.aux[StructuralSupportData].supportGrid(v) = StructuralSupportData.pack(6,1)
										allChanges ::= v
									}
								}
							}
						}
						case None =>
					}
				}
				case KeyPressEvent(key,modifiers) => {
					if (key == Keyboard.KEY_R) {
						IOPrelude.writeToFile( ResourceManager.getSaveFile("SSGCExperiment_record") , allChanges )
					} else if (key == Keyboard.KEY_P){
						allChanges = IOPrelude.readFromFile[List[VoxelCoord]]( ResourceManager.getSaveFile("SSGCExperiment_record") )
						world.aux[TerrainData].materialGrid.modificationBlock(allChanges) {
							for (v <- allChanges.reverse) {
								world.aux[TerrainData].setMaterialAt(v,CommonMaterials.Stone)
								world.aux[StructuralSupportData].supportGrid(v) = StructuralSupportData.pack(6,1)
							}
						}
					} else if (key == Keyboard.KEY_B) {
						for (v <- allChanges) { world.aux[StructuralSupportData].supportGrid(v) = StructuralSupportData.pack(6,1) }
						var found = true
						while (found) {
							found = false
							for (v <- allChanges) { found ||= structComp.updateSupport(v) }
						}
					} else if (key == Keyboard.KEY_1) {
						activeMaterial = CommonMaterials.Stone
					} else if (key == Keyboard.KEY_2) {
						activeMaterial = CommonMaterials.Wood
					} else if (key == Keyboard.KEY_3) {
						activeMaterial = Material.withName("iron")
					} else if (key == Keyboard.KEY_4) {
						activeMaterial = Material.withName("soil")
					}
				}
			}
		})
	}

	var allChanges = List[VoxelCoord]()

	val textures = Map( CommonMaterials.Stone -> image("axis/entities/materials/textures/stone_constructed.png"),
								CommonMaterials.Wood -> image("axis/entities/materials/textures/wood_constructed_quality.png"),
								Material.withName("iron") -> image("axis/entities/materials/textures/metal.png"),
								Material.withName("soil") -> image("axis/entities/materials/textures/tilled_soil.png") )
		.map(t => t._1 -> ImageUtils.toGrayscaleImage(t._2))

	override def setUpGraphicsEngine(): Unit = {
		graphicsEngine.pov = graphicsEngine.pov match {
			case ac : AnthologiconCamera => ac
			case _ => new AnthologiconCamera()
		}
		graphicsEngine.pov.asInstanceOf[AnthologiconCamera].turnSpeed = Vec2f(0.75f)
		graphicsEngine.pov.asInstanceOf[AnthologiconCamera].moveSpeed = Vec3f(0.5f)
		graphicsEngine addComponent new ShinDynamicGraphicsComponent {
			lazy val shader = GameUIShader.fetch(world,graphicsEngine)
			override def update(dt: UnitOfTime, bucket: RenderBucket): Unit = {
//				val Light = LightResult(1.0f,0.0f,0)

				val TD = gameEngine.world.aux[TerrainData]
				val SSD = gameEngine.world.aux[StructuralSupportData]
//				val tc = bucket.textureBlock(image("axis/entities/materials/textures/bordered.png"))
				for (x <- -15 to 15 optimized; y <- -15 to 15 optimized ; z <- -1 to 50 optimized ) {
					val v = VoxelCoord.Center + Vec3i(x,y,z)
					if (TD.isSolid(v)) {
						val mat = TD.materialAt(v)
						val tc = bucket.textureBlock( textures(mat) )
						val sval = SSD.supportGrid(v)

						val maxS = ssl.materialInfoFor(mat).effVerticalSupport
						val color = sval match {
							case 0 => Vec4f.One
							case _ => {
								StructuralSupportData.unpackSupport(sval) match {
									case 0 => Vec4f.One
									case 1 => Vec4f(0.1f,0.1f,0.1f,1.0f)
									case s => {
										val f = s.toFloat / maxS
										MathPrelude.linInterpolatev4(f,List(0.0f -> Color.Red, 1.0f -> Color.Green)).min(Vec4f.One)
									}
								}
							}
						}

						var faces = 0
						for (i <- 0 until 6 optimized) {
							if (! TD.isSolid(v + dirvec(i))) { faces |= (1 << i) }
						}
//						CommonRendering.drawCube(bucket,v.toObjectCoord,Vec3f.One,color,CommonRendering.passthroughLightResult,tc,Vec2i.One,faces)
						CommonRendering.drawCube(bucket,v.toObjectCoord,Vec3f.One,color,CommonRendering.passthroughLightResult,tc,Vec2i.One,CommonRendering.allFaces)
					}
				}
			}
			override def bucketIdentifier: Symbol = 'sym
			override def bucketRequirements = RenderBucketRequirements(UIAttributeProfile,shader)
		}
	}
}
