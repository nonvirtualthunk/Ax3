package arx.tmp.game.logic.mythology

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/18/14
 * Time: 8:19 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.language.semantic.Word
import arx.core.representation.ConfigValue
import arx.tmp.game.logic.descriptors.TDescriptor
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.data.TConfigurableGameEntityAuxData
import arx.tmp.game.logic.entities.data.TGameEntityAuxData
import arx.tmp.game.logic.universe.Universe
import arx.tmp.game.logic.universe.data.DeityData
import arx.tmp.game.procedural.writing.THasProcWritingTokens

import scala.language.implicitConversions

class MythologicalEntity extends GameEntity {
	aux[MythologicalEntityData]
}

object MythologicalEntity {
	implicit def toData (me : MythologicalEntity) : MythologicalEntityData = me.aux[MythologicalEntityData]
}

class MythologicalEntityData extends TConfigurableGameEntityAuxData with THasProcWritingTokens {
	var kind = "generic mythological entity"
	var flags = Set[String]()
	var titles = List[Title]()
	var pantheon = Pantheon.Universal

	def tokensFromPantheon = {
		val mainTokens = pantheon.deityWritingTokens.tokens
			.filter(_.conditions.forall(_.matches(this)))
			.flatMap(_.tokens)
			.toMap
		mainTokens + ("Pantheon" -> pantheon.name)
	}

	def tokensFromTitles = {
		val mainTitle = if (titles.isEmpty) { Title.Sentinel } else { titles.head }
		val altTitle = if (titles.size < 2) { mainTitle } else { titles(1) }

		Map(
			"Title" -> mainTitle.titleString,
			"DefiniteTitle" -> mainTitle.definiteTitleString,
			"AlternateTitle" -> altTitle.titleString,
			"DefiniteAlternateTitle" -> altTitle.definiteTitleString
		)
	}

	override def writingTokens: Map[String, String] = 
		super.writingTokens ++ tokensFromPantheon ++ tokensFromTitles


	override def createFromSML(conf: ConfigValue): Option[TGameEntityAuxData] = {
		if (conf.hasField("pantheon") || conf.hasField("title") || conf.hasField("titles")) {
			val ret = new MythologicalEntityData
			ret.kind = conf.kind.strOrElse(ret.kind)
			val titlesConf = conf.titles.arr
			if (titlesConf.nonEmpty) { Noto.warn("Titles provided in mythological entity data conf, but no impl for that yet") }
			if (conf.pantheon.nonEmpty) {
				Universe.universe.aux[DeityData].pantheonWithName(conf.pantheon.str) match {
					case Some(panth) => ret.pantheon = panth
					case None => Noto.warn(s"Could not find pantheon named ${conf.pantheon.str}")
				}
			}
			ret.flags = extractSingularOrPlural(conf,"flag","flags").map(_.str).toSet
			Some(ret)
		} else { None }
	}
}



trait TMythologicalEntitySubGenerator {
	def generateEntityWithDescriptor (description : TDescriptor, pantheon : Pantheon) : Option[MythologicalEntity]
}



case class MythologicalFeature (description : List[Word]) {

}



