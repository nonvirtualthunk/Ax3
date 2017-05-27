package arx.bol.game.entities

/**
  * TODO: Add javadoc
  */

import arx.Prelude._

import scalaxy.loops._
import ItemArchetype._
import arx.bol.game.entities.data.DamageType
import arx.bol.game.entities.data.Hand
import arx.bol.game.entities.data.Rectangle
import arx.bol.game.entities.data.ShapeUnion
import arx.core.introspection.TEagerSingleton
import arx.core.vec.Vec2i

object Weapons extends TEagerSingleton {
	val BronzeSword = new WeaponArchetype("Bronze Sword")
	BronzeSword.shape = ShapeUnion(Rectangle(1,6),Vec2i(1,0), Rectangle(3,1), Vec2i(0,1))
	BronzeSword.damageTypes = List(DamageType.Slashing, DamageType.Piercing)
	BronzeSword.bodySlotKind = Hand
	BronzeSword.baseDamage = 3
	BronzeSword.speedModifier = 0.75f

	val BronzeDagger = new WeaponArchetype("Bronze Dagger")
	BronzeDagger.shape = Rectangle(1,3)
	BronzeDagger.damageTypes = List(DamageType.Slashing, DamageType.Piercing)
	BronzeDagger.bodySlotKind = Hand
	BronzeDagger.baseDamage = 3
	BronzeDagger.speedModifier = 0.75f
}
