package arx.tmp.game.logic.mythology

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/13/14
 * Time: 4:05 PM
 */

import arx.core.Moddable
import arx.core.language.LanguageData.Language
import arx.tmp.game.logic.descriptors.ConfiguredDescriptor
import arx.tmp.game.logic.descriptors.TDescriptor
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.mythology.Pantheon.WritingTokenGroup
import arx.resource.ResourceManager

class Pantheon extends GameEntity {
	var deityWritingTokens = Moddable(WritingTokenGroup(Nil))
	var deities : Moddable[Set[Deity]] = Moddable(Set[Deity]())
	var mythologicalEntities = Set[MythologicalEntity]()
	var cardinality = Pantheon.Monotheistic
	var language : Language = Language.Sentinel
}

object Pantheon {
	val Monotheistic = 1
	val Polytheistic = 2
	val Multitheistic = 3
	val Animistic = 4

	case class WritingTokenPairs (conditions : List[TDescriptor], tokens : Map[String,String])
	case class WritingTokenGroup (tokens : List[WritingTokenPairs])
	val writingTokenGroups : List[WritingTokenGroup] = {
		var ret = List[WritingTokenGroup]()
		val base = ResourceManager.sml("axis/entities/deities/Pantheon.sml")

		val tokenGroupsObj = base.Arx.Pantheon.WritingTokenGroups

		// Parse out each token group into a collection of conditions -> tokens pairs
		for (tokenGroup <- tokenGroupsObj.arr) {
			var tokens = List[WritingTokenPairs]()
			for ((_,groupContainer) <- tokenGroup.fields) {
				var pairs = Map[String,String]()
				val conditions = extractSingularOrPlural(groupContainer,"condition","conditions")
					.toList
					.map(s => ConfiguredDescriptor(s))
				for ((key,value) <- groupContainer.fields if key != "condition" && key != "conditions") {
					pairs += key -> value.str
				}
				tokens ::= WritingTokenPairs(conditions,pairs)
			}
			ret ::= WritingTokenGroup(tokens)
		}
		ret
	}

	val Universal = new Pantheon
	val tmp : Moddable[WritingTokenGroup] = writingTokenGroups.headOption match {
		case None => WritingTokenGroup (Nil)
		case Some (wtg) => wtg
	}
	Universal.deityWritingTokens = tmp
}