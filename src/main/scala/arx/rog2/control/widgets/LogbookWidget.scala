package arx.rog2.control.widgets

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 6/7/17
  * Time: 6:51 AM
  */

import arx.Prelude._
import arx.core.Moddable

import scalaxy.loops._
import arx.core.vec._
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.widgets.{BottomRight, PositionExpression, TextDisplayWidget}
import arx.engine.entity.TGameEntity
import arx.engine.world.World
import arx.graphics.Image
import arx.graphics.helpers._
import arx.rog2.game.actions.DamageDone
import arx.rog2.game.data.entity.DieRoll
import arx.rog2.game.data.world.Logbook
import arx.rog2.game.events.Rog

class LogbookWidget(widg : Widget, world : World) extends TextDisplayWidget(widg) {
	y = PositionExpression.Constant(0, BottomRight)
	drawing.drawBackground = false
	fontScale = 2.0f


	text = Moddable(() => world[Logbook].messages
		.toStream
		.filter(m => m.level.ordinal > Rog.Fine.ordinal)
		.take(5)
		.reverse
		.map(msg => {
			var tmp = msg.text
			var i = 0
			var activeStr = ""
			var sections = Vector[RichTextSection]()

			while (i < tmp.length) {
				tmp(i) match {
					case '@' if i < tmp.length - 1 =>
						val refIndex = Integer.parseInt(tmp(i+1).toString)
						if (refIndex < msg.references.size) {
							val ref = msg.references(refIndex)
							// add the normal string so far as a section
							sections :+= TextSection(activeStr)
							activeStr = ""
							// then add a section for the referenced thing
							sections ++= toRichTextSection(ref)
						} else {
							activeStr += "?"
						}
						i += 1
					case o =>
						activeStr += o
				}

				i += 1
			}
			sections :+= TextSection(activeStr)
			sections
		}).reduceLeftOption((l,s) => l.:+(TextSection("\n")).++(s))
//		.map(sections => {
//			var ret = Vector[RichTextSection]()
//			for (s <- sections if s.symbolCount > 0) {
//				if (ret.nonEmpty) {
//					ret :+= s
//				} else {
//					ret.last.merge(s) match {
//						case Some(merged) => ret = ret.dropRight(1) :+ merged
//						case None => ret :+= s
//					}
//				}
//			}
//			ret
//		})
		.map(s => RichText(s))
		.getOrElse(RichText.Empty)
			//			}).reduceLeftOption(_ + "\n" + _)
			//			.getOrElse(RichText.Empty)
		)




	def toRichTextSection(ref : Any) : Seq[RichTextSection] = ref match {
		case ent : TGameEntity => Vector(TextSection(ent.name, Color.Green))
		case DieRoll(result, size) =>
			Vector(ImageSection(List(ImageSectionLayer("rog/ui/icons/dice/d6_base.png", Color.Red), ImageSectionLayer(s"rog/ui/icons/dice/d6_$result.png")),1.5f))
		case DamageDone(dice, bonus) =>
			dice.flatMap(d => toRichTextSection(d))
		case o =>
			Vector(TextSection(o.toString))
	}
}
