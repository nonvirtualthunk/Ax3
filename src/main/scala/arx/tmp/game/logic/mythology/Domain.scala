package arx.tmp.game.logic.mythology

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/13/14
 * Time: 8:12 PM
 */

import arx.core.representation.ConfigValue
import arx.core.representation.TConfigurable
import arx.tmp.game.logic.entities.core.GameArchetype
import arx.tmp.game.logic.mythology.DeityDescriptors.DeityWithDomainDescriptor


class Domain extends GameArchetype with TConfigurable {
	override def setFromSML(sml: ConfigValue, overwrite: Boolean): Unit = {
		name = sml.name.strOrElse(name)
		for (t <- extractSingularOrPlural(sml,"title","titles")) {
			val title = new TitleArchetype
			title.name = t.str
			title.conditions = List(new DeityWithDomainDescriptor(name::Nil))
			Deity.titles ::= title
		}
	}
}
