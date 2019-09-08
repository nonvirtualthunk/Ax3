package arx.rog2.game.entities

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 6/5/17
  * Time: 8:01 AM
  */

import arx.Prelude._
import arx.core.MutableModdable

import scalaxy.loops._
import arx.core.vec._
import arx.engine.entity.GameEntity
import arx.rog2.game.data.entity._

object Weapons {
	protected implicit def toMM(f : Float) : MutableModdable = MM(f)
	protected implicit def toMM(f : Int) : MutableModdable = MM(f)


	def shortSword = {
		val ent = new GameEntity("Short Sword")
		ent[Weapon].withData(w => {
			w.accuracy = 1.0f
			w.attackDuration = 0.9f
			w.range = 1

			w.damageDice = 2
			w.damagePerDie = 6
			w.damageMultiplier = 1.0f
			w.damageBonus = 1
		})
		ent[Physical].withData(d => {
			d.effectiveOpacity = 0.1f
			//d.dimensions = 0.25.meters x 0.1.meters x 1.meter
			d.dimensions = 1.voxel x 1.voxel x 1.voxels
			d.drawInfo = TextureDrawInfo.apply("sword")
		})
		ent[Item].withData(d => {
			d.maxDurability = 30
			d.durabilityPerUse = 0.1f
		})
		ent[Equippable].allowedSlots += BodySlotType.Hand

		ent
	}
}
