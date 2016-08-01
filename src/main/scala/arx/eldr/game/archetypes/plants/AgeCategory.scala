package arx.eldr.game.archetypes.plants

/**
  * Age categories for various kinds of living things
  */

//noinspection NameBooleanParameters
object AgeCategory {

	object Tree {
		val Seedling = AgeCategory("Seedling", 0, "[Entity] [AgeCategory]", false)
		val Sapling = AgeCategory("Sapling", 1, "[Entity] [AgeCategory]", false)
		val Mature = AgeCategory("Mature", 2, "[AgeCategory] [Entity]", true)
		val Old = AgeCategory("Old", 3, "[AgeCategory] [Entity]", true)

		val AgeCategories = List(Seedling,Sapling,Mature,Old)
	}

	object Bush {
		val Shoot = AgeCategory("Shoot", 0, "[Entity] [AgeCategory]", false)
		val Blooming = AgeCategory("Blooming", 1, "[AgeCategory] [Entity]", false)
		val Mature = AgeCategory("Mature", 2, "[AgeCategory] [Entity]", true)

		val AgeCategories = List(Shoot,Blooming,Mature)
	}

	def apply(name: String, ordinal: Int, phraseRepresentation: String, isMature : Boolean = false): AgeCategory = {
		val ret = new AgeCategory(name)
		ret.phraseRepresentation = phraseRepresentation
		ret.ordinal = ordinal
		ret.isMature = isMature
		ret
	}
}

class AgeCategory(val name: String) {
	var phraseRepresentation: String = name
	var ordinal = 0
	var isMature = false

	override def toString: String = name
	override def equals(obj: scala.Any): Boolean = obj match {
		case ac : AgeCategory => ac.name == this.name
		case _ => false
	}
	override def hashCode(): Int = name.hashCode
}