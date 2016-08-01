package arx.slime.game.data

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.Moddable
import arx.core.Modifier
import arx.core.MutableModdable
import arx.core.TModifierSource
import arx.core.traits.ArxEnum
import arx.core.traits.ArxEnumObject
import arx.core.traits.TSentinel
import arx.core.vec.ReadVec4f
import arx.core.vec.Vec4f
import arx.engine.data.TGameEntityAuxData
import arx.engine.entity.GameArchetype
import arx.engine.entity.TArchetypeKind
import arx.engine.entity.TGameEntity

import scalaxy.loops._

class GeneticData extends TGameEntityAuxData {
	var chromosomes = List[ChromosomePair]()
}

case class ChromosomePair (left : Chromosome, right : Chromosome)

class Chromosome {
	var gene = Map[GeneType, Gene]()
}
class Gene (val allele : Allele, var expressionStrength : Moddable[Float]) extends TModifierSource {

}


class GeneType(nomen : String) extends ArxEnum(nomen)
object GeneType extends ArxEnumObject[GeneType] {
	val Baseline = GeneType("baseline")
	val Coloration = GeneType("coloration")
	val SlimeMuscles = GeneType("slime muscles")
	val Respiration = GeneType("respiration")
}

abstract class Allele(nomen : String, val isDominant : Boolean, val geneType : GeneType) extends GameArchetype(nomen, Allele) {
	def applyToEntity (gene : Gene, entity : TGameEntity, strength : Moddable[Float])
}

object Allele extends TArchetypeKind{
	val Sentinel : Allele = new Allele("Sentinel", false, GeneType.Baseline) with TSentinel {
		override def applyToEntity(gene : Gene, entity: TGameEntity, strength : Moddable[Float]) {}
	}
}

/**
  * Placeholder for situations in which the baseline we want for a given gene type is just to do nothing
  */
object BaselineAllele extends Allele("baseline", false, GeneType.Baseline) {
	override def applyToEntity(gene: Gene, entity: TGameEntity, strength: Moddable[Float]): Unit = {}
}



object RedColoration extends Allele("red pigmentation", true, GeneType.Coloration) {
	override def applyToEntity(gene: Gene, entity: TGameEntity, strength: Moddable[Float]): Unit = {
		val CD = entity[CreatureData]
		CD.color = gene.createModifier[ReadVec4f](CD.color, c => Vec4f(1.0f,0.0f,0.0f,1.0f))
	}
}

object BlueColoration extends Allele("blue pigmentation", false, GeneType.Coloration) {
	override def applyToEntity(gene: Gene, entity: TGameEntity, strength: Moddable[Float]): Unit = {
		val CD = entity[CreatureData]
		CD.color = gene.createModifier[ReadVec4f](CD.color, c => Vec4f(0.0f,0.0f,1.0f,1.0f))
	}
}



object StrongSlime extends Allele("strong muscles", true, GeneType.SlimeMuscles) {
	override def applyToEntity(gene : Gene, entity: TGameEntity, expressionStrength: Moddable[Float]) {
		val CD = entity[CreatureData]
		CD.strength = gene.createModifier[Int](CD.strength, s => s + expressionStrength.toInt)
		CD.landMovement = gene.createModifier[Int](CD.landMovement, m => m - (expressionStrength * 0.5f).toInt)
	}
}

object QuickSlime extends Allele("quick muscles", true, GeneType.SlimeMuscles) {
	override def applyToEntity(gene : Gene, entity: TGameEntity, expressionStrength: Moddable[Float]) {
		val CD = entity[CreatureData]
		CD.landMovement = gene.createModifier[Int](CD.landMovement, m => m * (1.0f + expressionStrength * 0.25f).toInt)
		CD.waterMovement = gene.createModifier[Int](CD.waterMovement, m => m * (1.0f + expressionStrength * 0.25f).toInt)
		CD.maxHP = gene.createModifier[Int](CD.maxHP, s => s - (expressionStrength * 0.75f).toInt)
	}
}

object RudimentaryLungs extends Allele("rudimentary lungs", false, GeneType.Respiration) {
	override def applyToEntity(gene : Gene, entity: TGameEntity, expressionStrength: Moddable[Float]) {
		val CD = entity[CreatureData]
		CD.landSurvival = gene.createModifier[Float](CD.landSurvival, m => math.max(m, 0.25f))
		CD.landMovement = gene.createModifier[Int](CD.landMovement, s => s * (1.0f + expressionStrength * 0.2f).toInt)
	}
}


object FullLungs extends Allele("full lungs", false, GeneType.Respiration) {
	override def applyToEntity(gene : Gene, entity: TGameEntity, expressionStrength: Moddable[Float]) {
		val CD = entity[CreatureData]
		CD.landSurvival = gene.createModifier[Float](CD.landSurvival, m => math.max(m, 1f))
		CD.landMovement = gene.createModifier[Int](CD.landMovement, s => s * (1.0f + expressionStrength * 0.3f).toInt)
	}
}

// We might want something less freeform for physical expressions like this, it would be handy to be able to change
// the base value of certain traits, as opposed to stacking things on. Perhaps that could be done within the existing
// framework though. Yeah, WrappingModdable exposes what is wrapped, so we could theoretically track it down to that
// and ... swap it out or something