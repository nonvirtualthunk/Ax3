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
import arx.engine.control.components.windowing.widgets.BottomRight
import arx.engine.control.components.windowing.widgets.DimensionExpression
import arx.engine.control.components.windowing.widgets.PositionExpression
import arx.engine.control.components.windowing.widgets.TextDisplayWidget
import arx.engine.control.components.windowing.widgets.data.DrawingData
import arx.engine.control.data.WindowingData
import arx.engine.control.event.Event.Event
import arx.engine.control.event.Event.KeyPressEvent
import arx.engine.entity.TGameEntity
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

class RogCharacterControl(engine: ControlEngine,
								  physics: RogPhysicsGameComponent,
								  creatureComp : RogCreatureGameComponent) extends ControlComponent(engine) with RogComponent
{
	dependencies ::= classOf[RogPhysicsGameComponent]
	var lastMove = 0.seconds
	val gapBetweenRepeat = 0.1.seconds


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

		val text = new TextDisplayWidget(widg)
		text.y = PositionExpression.Constant(0, BottomRight)
		text.drawing.drawBackground = false
		text.text = Moddable(() => world[Logbook].messages
			.toStream
			.filter(m => m.level.ordinal > Rog.Fine.ordinal)
			.take(5)
			.reverse
			.map(msg => {
				var tmp = msg.text
				for ((ref,i) <- msg.references.zipWithIndex) {
					tmp = tmp.replaceAllLiterally("@" + i, stringify(ref))
				}
				tmp
			}).reduceLeftOption(_ + "\n" + _)
			.getOrElse("")
		)
		text.fontScale *= 2.0f

	}


	def stringify(ref : Any) = ref match {
		case ent : TGameEntity => ent.name
		case o => o.toString
	}


	controlEvents.onEvent {
		case KeyPressEvent(key, modifiers, repeat) => {
			if (!repeat || (curTime() - lastMove) > gapBetweenRepeat) {
				lastMove = curTime()
				val movement = key match {
					case GLFW.GLFW_KEY_W => Vec3i(0, 1, 0)
					case GLFW.GLFW_KEY_S => Vec3i(0, -1, 0)
					case GLFW.GLFW_KEY_A => Vec3i(-1, 0, 0)
					case GLFW.GLFW_KEY_D => Vec3i(1, 0, 0)
					case GLFW.GLFW_KEY_Q => Vec3i(-1, 1, 0)
					case GLFW.GLFW_KEY_E => Vec3i(1, 1, 0)
					case GLFW.GLFW_KEY_C => Vec3i(1, -1, 0)
					case GLFW.GLFW_KEY_Z => Vec3i(-1, -1, 0)
					case _ => Vec3i.Zero
				}

				if (movement != Vec3i.Zero) {
					val PD = world[RogData].player[Physical]
					val targetPos = PD.position + movement

					physics.entitiesAtLocation(targetPos).filter(e => e[Physical].solid) match {
						case Nil =>
							val T = world[Terrain]
							if (T.voxel(targetPos).isSentinel) {
								creatureComp.executeAction(MoveAction(player, PD.position, targetPos))
							} else if (T.voxel(targetPos.plusZ(1)).isSentinel) {
								creatureComp.executeAction(MoveAction(player, PD.position, targetPos.plusZ(1)))
							}
						case entities =>
							val intersectedEntity = entities.head
							if (intersectedEntity.hasAuxData[Creature]) {
								creatureComp.executeAction(AttackAction(player, intersectedEntity))
							} else {
								creatureComp.executeAction(InteractAction(player, intersectedEntity))
							}
					}
				} else {
					key match {
						case GLFW.GLFW_KEY_P =>
							for (heldItem <- player[Inventory].heldItems.headOption) {
								val placePos = player[Physical].position + Cardinals.dirvec(player[Physical].facing)
								creatureComp.executeAction(PlaceItemAction(player, heldItem, placePos))
							}
						case _ =>
					}
				}
			}
		}
	}
}

case class AdvanceWorldEvent(dt: UnitOfTime) extends Event