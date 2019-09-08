package arx.tyche.game.data

import arx.engine.entity.{HasIdentity, Identity, TGameEntity, Taxon}
import arx.engine.world.World

abstract class SourceType(val identity : Identity) extends HasIdentity {
	var baseFood = 0
	var baseTech = 0
	var baseProd = 0

	var baseNatura = 0

	var baseRange = 0
	var baseNaturaRange = 0
	var baseMaximumAspects = 2

	var animationFrames = 1

	def food(patch : TGameEntity) = baseFood
	def tech(patch : TGameEntity) = baseTech
	def prod(patch : TGameEntity) = baseProd

	def natura(patch : TGameEntity) = baseNatura

	def range(patch : TGameEntity) = baseRange
	def naturaRange(patch : TGameEntity) = baseNaturaRange

	def maximumAspects(patch : TGameEntity) = baseMaximumAspects

	def name = identity.taxons.head.name
}


object SourceType {
	case object None extends SourceType(Taxon("barren", Taxonomy.SourceType)) {
		baseMaximumAspects = 0
	}

	case object Mackerel extends SourceType(Taxon("mackerel", Taxonomy.Source.Fish)) {

		baseRange = 1
		baseFood = 2

		animationFrames = 4

		override def range(patch : TGameEntity) = {
			var r = 0
			var newR = baseRange

			while (r != newR) {
				r = newR
				val schoolSize = patch[Patch].patchesWithinRange(r).count(e => e[Patch].sourceType == Mackerel)
				newR = schoolSize.max(1)
			}
			r
		}
	}
}