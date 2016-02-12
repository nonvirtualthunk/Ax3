package arx.tmp.game.procedural.writing

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/14/14
 * Time: 7:59 AM
 */

import java.util.regex.Pattern

import arx.Prelude._
import arx.application.Noto
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.data.TAuxData

import scala.collection.mutable

object ProceduralWriting {
	def generateFromTemplateString (template : String, entity : GameEntity, auxTokens : Map[String,String]) :String = {
		generateFromTemplate(compileTemplate(template),entity,auxTokens)
	}

	def generateFromTemplate (template : Template, entity : GameEntity, auxTokens : Map[String,String]) : String = {
		generateFromTemplate(template,entity,Map.empty,auxTokens)
	}

	protected def toCaseInsensitive[A] (map : Map[String,A]) = {
		map.map { case (k,v) => k.toLowerCase -> v }
	}

	def extractWritingTokens(auxData : List[TAuxData]) : Map[String,String] = {
		val mapped = auxData.map {
			case proc : THasProcWritingTokens => proc.writingTokens
			case _ => Map[String,String]()
		}
		if (mapped.nonEmpty) {
			mapped.reduce((m1,m2) => m1 ++ m2)
		} else {
			Map()
		}
	}
	def extractWritingTokens(entity : GameEntity) : Map[String,String] =
		extractWritingTokens(entity.allAuxiliaryData) ++ (entity match {
			case proc : THasProcWritingTokens => proc.writingTokens
			case _ => Map[String,String]()
		})

	def generateFromTemplate (
		template : Template,
		mainEntity : GameEntity,
		entities_ : Map[String,GameEntity],
		auxTokens_ : Map[String,String]
	) : String = {
		if (template.replacementTargets.isEmpty) {
			template.string
		} else {
			val entities = toCaseInsensitive(entities_)
			val auxTokens = toCaseInsensitive(auxTokens_)
			val mainTokens = toCaseInsensitive( extractWritingTokens(mainEntity) )
			val cachedTokens = new mutable.HashMap[String,Map[String,String]]
			var ret = template.string

			var cuid = mainEntity match {
				case ge: GameEntity => ge.uid
				case _ => mainEntity.hashCode ()
			}

			for (targetString <- template.replacementTargets if targetString.length > 2) {
				val replacement = targetString.substring (1, targetString.length - 1)
				val replacementParts = replacement.split (" ")
				val fromTokens = if (replacementParts.length == 1) {
					mainTokens
				} else if (replacementParts.length == 2) {
					val entName = replacementParts(0).toLowerCase
					entities.get(entName) match {
						case Some(ent) =>
							cachedTokens.getOrElseUpdate(entName,toCaseInsensitive( extractWritingTokens(ent) ))
						case None =>
							Noto.warn("Two part replacement selector, \"" + targetString + "\", no entity for initial part found")
							Map()
					}
				} else {
					Noto.warn("Multi part replacement selector, \"" + targetString + "\" with invalid number of parts")
					Map()
				}

				val tokenSelector = replacementParts.last
				val effectiveTokenSelector = if (tokenSelector.contains ("/")) {
					val possibleSelectors = replacement.split ("/")
					val ret = possibleSelectors((cuid.abs % possibleSelectors.length).toInt)
					cuid *= 13 // "randomize" it a bit
					ret
				} else {
					tokenSelector
				}

				val replaceWith = (fromTokens ++ auxTokens).get(effectiveTokenSelector.toLowerCase) match {
					case Some(repl) => repl
					case None => entities.get(effectiveTokenSelector.toLowerCase) match {
						case Some(ent) => ent.name
						case _ => "[]"
					}
				}

				ret = ret.replaceAllLiterally(targetString,replaceWith)
			}
			postProcess(ret)
		}
	}
	
	protected def postProcess (baseString : String) = {
		val sb = new StringBuilder
		for ( i <- 0 until baseString.size ) {
			if ( i >= 2 && baseString(i - 2) == '.' && baseString(i - 1) == ' '  ) {
				sb.append( baseString(i).toUpper )
			} else { sb.append( baseString(i) ) }
		}
		sb.toString()
	}

	def compileTemplate (templateString : String) = {
		val replacements = templateString.extract(replacementPattern)
		Template(templateString, replacements)
	}

	val replacementPattern = Pattern.compile("(\\[.*?\\])")


	case class Template (string : String, replacementTargets : List[String]) {
		def generate(entity : GameEntity with THasProcWritingTokens,auxTokens : Map[String,String]) {
			ProceduralWriting.generateFromTemplate(this,entity,auxTokens)
		}
	}
}
