package arx.tmp.game.logic.universe.data

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/14/13
 * Time: 4:22 PM
 * Created by nonvirtualthunk
 */

import arx.core.language.LanguageData.Language
import arx.tmp.game.logic.universe.TUniverseData

@SerialVersionUID(1L)
class LanguageData extends TUniverseData {
	var languages : List[Language] = Nil
}