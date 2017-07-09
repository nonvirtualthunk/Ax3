package arx.rog2.control

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.Moddable
import arx.core.units.UnitOfTime
import arx.core.vec.Cardinals
import arx.core.vec.Vec3i
import arx.engine.control.ControlEngine
import arx.engine.control.components.ControlComponent
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.widgets.DimensionExpression.Intrinsic
import arx.engine.control.components.windowing.widgets.ImageDisplayWidget.ActualSize
import arx.engine.control.components.windowing.widgets.PositionExpression.Constant
import arx.engine.control.components.windowing.widgets._
import arx.engine.control.components.windowing.widgets.data.DrawingData
import arx.engine.control.data.WindowingData
import arx.engine.control.event.Event.Event
import arx.engine.control.event.Event.KeyPressEvent
import arx.engine.entity.{GameEntity, TGameEntity}
import arx.graphics.helpers.{Color, RichText}
import arx.rog2.control.modes.{ControlMode, InventoryMode, MainMode}
import arx.rog2.control.widgets.LogbookWidget
import arx.rog2.engine.RogComponent
import arx.rog2.game.actions.Action
import arx.rog2.game.actions.AttackAction
import arx.rog2.game.actions.InteractAction
import arx.rog2.game.actions.MoveAction
import arx.rog2.game.actions.PlaceItemAction
import arx.rog2.game.data.entity.Creature
import arx.rog2.game.data.entity.Inventory
import arx.rog2.game.data.entity.Physical
import arx.rog2.game.data.world.{Logbook, RogData, Terrain}
import arx.rog2.game.engine.{RogCreatureGameComponent, RogPhysicsGameComponent}
import arx.rog2.game.events.Rog
import org.lwjgl.glfw.GLFW
import org.lwjgl.glfw.GLFW._

class RogCharacterControl(engine: ControlEngine,
								  val physics: RogPhysicsGameComponent,
								  val creatureComp : RogCreatureGameComponent) extends ControlComponent(engine) with RogComponent
{
	dependencies ::= classOf[RogPhysicsGameComponent]

	var modeStack = List[ControlMode]()
	pushMode(new MainMode)


	override protected def initialize(): Unit = {
		val desktop = control[WindowingData].desktop

//		desktop[DrawingData].drawBackground = false
		desktop[DrawingData].withData { o =>
			o.drawAsForegroundBorder = true
		}

		val widg = new Widget(desktop)
		widg.width = DimensionExpression.Proportional(1.0f)
		widg.height = DimensionExpression.Constant(200)
		widg.y = PositionExpression.Constant(0, BottomRight)
		widg[DrawingData].withData { o =>
			o.backgroundImage = Some("ui/minimalistBorder_ne.png")
		}

		val text = new LogbookWidget(widg, world)
//		val text = new TextDisplayWidget(widg)
//		text.y = PositionExpression.Constant(0, BottomRight)
//		text.drawing.drawBackground = false
//		text.text = Moddable(() => world[Logbook].messages
//			.toStream
//			.filter(m => m.level.ordinal > Rog.Fine.ordinal)
//			.take(5)
//			.reverse
//			.map(msg => {
//				var tmp = msg.text
//				var i = 0
//				var activeStr = ""
//				while (i < tmp.length) {
//
//
//					i += 1
//				}
//
//				for ((ref,i) <- msg.references.zipWithIndex) {
//					tmp = tmp.replaceAllLiterally("@" + i, stringify(ref))
//				}
//				tmp
//				RichText.Empty
//			})
////			}).reduceLeftOption(_ + "\n" + _)
////			.getOrElse(RichText.Empty)
//		)
//		text.fontScale = 2.0f


		val statusDisplay = {
			val w = new Widget(desktop)
			w.x = PositionExpression.Constant(0, TopRight)
			w.y = PositionExpression.Constant(0, TopRight)
			w.width = DimensionExpression.Constant(250)
			w.height = DimensionExpression.Constant(250)
			w.drawing.backgroundImage = Some("ui/minimalistBorderWhite_ne.png")
			w.drawing.backgroundColor = Color(200,200,200,255)

			case class Stat(name : String, curFunc : (Creature) => Int, maxFunc : (Creature) => Int)
			val stats = Stat("food",c => c.food, c => c.maxFood) :: Stat("health", c => c.hp, c => c.maxHP) :: Stat("sanity", c => c.sanity, c => c.maxSanity) :: Nil
			val displays = stats.map(s => {
				val iconDisp = new ImageDisplayWidget(w)
				iconDisp.image = Moddable(s"rog/ui/icons/${s.name}Icon.png")
				iconDisp.scalingStyle = ActualSize(2)
				iconDisp.x = Constant(5)
				iconDisp.drawing.drawBackground = false

				val textDisp = new TextDisplayWidget(w)
				textDisp.x = PositionExpression.Relative(iconDisp, 10)
				textDisp.y = PositionExpression.Relative(iconDisp, 0, Cardinals.Center)

				textDisp.text = Moddable(() => s"${s.curFunc(player[Creature])} / ${s.maxFunc(player[Creature])}")
				textDisp.drawing.drawBackground = false
				textDisp.fontScale = 2.0f

				iconDisp
			})

			displays.head.y = PositionExpression.Constant(15)
			displays.sliding(2).foreach(pair => pair.last.y = PositionExpression.Relative(pair.head,10,Cardinals.Down))

			w
		}

	}


	def stringify(ref : Any) = ref match {
		case ent : TGameEntity => ent.name
		case o => o.toString
	}


	def popMode(): Unit = {
		if (modeStack.size > 1) {
			modeStack.head.close()
			modeStack = modeStack.tail
		}
	}

	def pushMode(mode : ControlMode): Unit = {
		mode.characterControl = this
		mode.initialize()
		modeStack ::= mode
	}

	controlEvents.onEvent {
		case KeyPressEvent(key,_,_) if key == GLFW_KEY_ESCAPE =>
			popMode()
		case kpe: KeyPressEvent=>
			var consumed = false
			var toCheck = modeStack
			while (toCheck.nonEmpty && !consumed) {
				consumed = toCheck.head.handleEvent(kpe)
				toCheck = toCheck.tail
			}
	}
}

case class AdvanceWorldEvent(dt: UnitOfTime) extends Event