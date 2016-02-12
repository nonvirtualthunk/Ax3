package arx.tmp.game.procedural.writing

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/14/14
 * Time: 8:07 AM
 */

import arx.tmp.game.logic.entities.core.GameArchetype
import arx.tmp.game.logic.entities.core.GameEntity

trait THasProcWritingTokens {
	/** Return a map of all replacement tokens that the implementing class can provide, joined to
	  * <code>super.writingTokens</code>. Tokens would be, e.g. for the Male sex, "pronoun" -> "he",
	  * "parent" -> "father", etc. */
	def writingTokens : Map[String,String] = this match {
		case ge : GameEntity => Map("Name" -> ge.name)
		case ga : GameArchetype => Map("Name" -> ga.name)
		case _ => Map()
	}
}
