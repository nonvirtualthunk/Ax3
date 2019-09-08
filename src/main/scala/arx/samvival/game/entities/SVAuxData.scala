package arx.samvival.game.entities

import arx.core.introspection.ReflectionAssistant
import arx.engine.data.TAuxData

trait SVAuxData extends TAuxData {
	override def toString() = {
		this.getClass.getDeclaredFields.map(f => {
			s"${f.getName} : ${ReflectionAssistant.getFieldValue(this, f)}"
		}).mkString("\n")
	}
}
