package arx.tmp.game.logic.mythology

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/6/14
 * Time: 9:26 AM
 */

import arx.Prelude._
import arx.tmp.game.logic.mythology.DeityDescriptors.DeityWithDomainDescriptor
import arx.tmp.game.logic.mythology.descriptors.MythologicalPlaceDescriptor
import arx.tmp.game.procedural.writing.ProceduralWriting

object BasicEventGenerator {
	val tricksterDescriptor = new DeityWithDomainDescriptor(List("trickery"))
	val hallDescriptor = MythologicalPlaceDescriptor(positive = true)


	def pick (from : Set[Deity],condition : Deity => Boolean) = {
		val possibles = from.filter(condition)
		if (possibles.isEmpty) {
			new Deity()
		} else {
			randFrom(possibles)
		}
	}

	def generateEvent (pantheon : Pantheon) = {
		var rd : Set[Deity] = pantheon.deities
		val subject = pick(rd,d => true); rd -= subject
		val trickster = pick(rd,tricksterDescriptor.matches); rd -= trickster
		val target = pick(rd,d => true); rd -= target

		
		val hall = MythologicalEntityGenerator.generateMythologicalEntity(hallDescriptor,pantheon)

		val str = """
In the days before the world was sundered [Name] attended a great banquet of the gods in the hall of [Hall].
There [pronoun] was pressed with many wines of surpassing sweetness. Among friends and fellows [pronoun]
allowed [reflexive] to be plyed and soon lost [possessive] clarity of thought to the persuasion
of the sweet liquor. Soon, [Trickster], spying an opportunity for mischief, was whispering in
[possessive] ear of the slights done to
[accusative] by [Target]. With lies and tales [Trickster DefiniteTitle] aroused [accusative] to outrage and fury.
Encouraged further by [Trickster Name] and a particularly strong draught
[Trickster pronoun] produced from some hidden pocket, [Name] arose and, stumbling, challenged [Target]
before the assembled gods. [Target], surprised, nevertheless accepted the challenge and so a duel
began. The drunken [Name] soon fell, battered, [Possessive] [Weapon] broken on the ground.
[Pronoun] dragged [Reflexive] away, bleary and beaten, and to this day has not forgiven
[Possessive] guileless opponent, [Target], for [Possessive] shame.
				"""

		val template = ProceduralWriting.compileTemplate(str)
		val procd = ProceduralWriting.generateFromTemplate(template,subject,Map("trickster" -> trickster,"target" -> target,"hall" -> hall),Map())

		println("===========================================")
		println(procd)
		println("===========================================")

		str
	}
}
