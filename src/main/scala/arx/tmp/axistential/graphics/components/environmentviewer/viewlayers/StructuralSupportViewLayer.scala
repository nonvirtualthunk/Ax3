package arx.axistential.graphics.components.environmentviewer.viewlayers

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/16/14
 * Time: 4:46 PM
 */

import arx.Prelude._
import arx.axistential.game.data.world.StructuralSupportData
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.logic.structuralsupport.TStructuralSupportLogic
import arx.axistential.graphics.components.renderers.TEnvironmentRenderer
import arx.axistential.graphics.shader.GameUIShader
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec3f
import arx.core.vec.Vec4f
import arx.engine.world.World
import arx.graphics.shader.TShader
import arx.graphics.AVBO
import arx.graphics.AttributeProfile
import arx.graphics.TextureBlock

import scalaxy.loops._

class StructuralSupportViewLayer extends TUIViewLayer {
	lazy val SSL = pio[TStructuralSupportLogic]

	override def revisionOf(env: World, loc: VoxelCoord): Long = {
		env.aux[StructuralSupportData].supportGrid.getModifiedCount(loc)
//		val all = env.aux[StructuralSupportData].supportGrid.getModifiedCount(loc)
//		var rev = 0l
//		for (q <- 0 until all.length optimized) { rev += all(q) }
//		rev
	}
	
	override def renderer: TEnvironmentRenderer = new TEnvironmentRenderer {
		import StructuralSupportData._
		override def attributeProfile: AttributeProfile = UIAttributeProfile
		override def updateTalea(vbo: AVBO, textureBlock: TextureBlock, world: World, wloc: VoxelCoord, offset: ReadVec3i, limit: ReadVec3i): Unit = {
			val SSD = world.aux[StructuralSupportData]
			if (SSD.supportGrid.definedAt(wloc.x,wloc.y,wloc.z)) {
				val TD = world.aux[TerrainData]

				val ox = wloc.x - VoxelCoord.Center.x
				val oy = wloc.y - VoxelCoord.Center.y
				val oz = wloc.z - VoxelCoord.Center.z
				val tc = textureBlock(image("default/blankBordered.png"))
				val supportTalea = SSD.supportGrid.taleaFor(wloc.x,wloc.y,wloc.z)
				val terrainTalea = TD.materialGrid.taleaFor(wloc.x,wloc.y,wloc.z)
				for (x <- offset.x until limit.x optimized ; y <- offset.y until limit.y optimized ; z <- offset.z until limit.z optimized ) {
					val struct = supportTalea(x,y,z)
					if (! struct.isRootBlock) {
						val support = struct.support
						val maxSupport = SSL.materialInfoFor(TD.materialForByte(terrainTalea(x,y,z))).effVerticalSupport
						val pcnt = support.toFloat / maxSupport.toFloat
						val color = Vec4f(1.0f - pcnt,pcnt,0.0f,0.5f)
						CommonRendering.drawCube(vbo,ObjectCoord(ox + x + 0.5f,oy + y + 0.5f,oz + z + 0.5f),Vec3f(1.1f),color,tc)
					}
				}
			}
		}
		
	}

	lazy val _shader = GameUIShader(graphicsEngine.gameEngine.world,graphicsEngine)
	override def shader: TShader = _shader
}
