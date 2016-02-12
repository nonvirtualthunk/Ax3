package arx.axistential.game.entities

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/16/13
 * Time: 9:50 AM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.axistential.game.archetypes.Material
import arx.axistential.game.data.helpers.MaterialComposition
import arx.axistential.game.entities.traits.PhysicalEntityData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.physics.CubeCollisionShape
import arx.core.GeneralityLevel
import arx.core.vec.Vec4f
import arx.tmp.game.logic.entities.core.GameEntity

class MaterialBlock(val material : Material) extends GameEntity with TPhysicalEntity with TStackableEntity {
	name = material.instanceName

	archetype = material

	override protected def createPhysData(): PhysicalEntityData = {
		val tmp = new MaterialBlockPhysicalData
		tmp.init(material)
		tmp
	}
}

class MaterialBlockPhysicalData extends PhysicalEntityData {
	def init (mat : Material): Unit = {
		blockMaterial = mat
		this.materialComposition = List(MaterialComposition(mat,1.0f))
		this.collisionShape = new CubeCollisionShape(oneVoxelCube)
	}
	var blockMaterial : Material = Material.Sentinel

	override def primaryMaterial = blockMaterial
}

object MaterialBlockGraphicsInfoProvider extends TGameEntityGraphicsStructor {
	def generalityLevel: GeneralityLevel.GeneralityLevel = GeneralityLevel.Specific

	override def rawGraphicsInfoFor(gameEntity: GameEntity): Option[GameEntityGraphicsInfo] = {
		gameEntity match {
			case mb : MaterialBlock => Some(BillboardGraphicsInfo(image("axis/entities/materials/textures/dirt.png")::Nil,Vec4f.One))
			case _ => None
		}
	}
}

/**
 * Any entity that does not have any significant individual identity, and so can be represented
 * with others of the same kind as a stack of N items
 */
trait TStackableEntity {

}

class ItemStack ( val item : TPhysicalEntity, var count : Int ) extends TPhysicalEntity {

}