package arx.samvival.control

import arx.Prelude._
import arx.ai.search.{Path, Pathfinder, Searcher}
import arx.core.datastructures.Watcher
import arx.core.introspection.FieldOperations.SettableField
import arx.core.metrics.Metrics
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.{AxialVec, AxialVec3}
import arx.engine.advanced.lenginepieces.LControlEngine
import arx.engine.control.components.LControlModeComponent
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.widgets.{BottomRight, DimensionExpression, PositionExpression, TopRight}
import arx.engine.control.components.windowing.widgets.data.DrawingData
import arx.engine.control.data.WindowingData
import arx.engine.control.event.Event.{KeyPressEvent, Mouse, MousePressEvent}
import arx.engine.lworld.LEntity
import arx.samvival.control.widgets.SideBar
import arx.samvival.game.actions.{AttackAction, GameAction, MoveAction, SwitchActiveCharacter}
import arx.samvival.game.entities.Fields.Physical
import arx.samvival.game.entities._
import arx.samvival.game.events.GameEvents
import arx.samvival.game.logic._
import arx.samvival.graphics.animation._
import arx.samvival.graphics.data.Selection
import org.lwjgl.glfw.GLFW

class MainMapControlModeComponent(_engine : LControlEngine) extends LControlModeComponent(_engine) with SamvivalControlComponent {

	val terrainData = world.view.dataStore[Terrain]
	val tileData = world.view.dataStore[Tile]
	val physicalData = world.view.dataStore[Physical]
	val vegData = world.view.dataStore[Vegetation]
	val pathfinder = Pathfinder[AxialVec3]("main map pathfinder",
		v => v.neighbors,
		(char, v1,v2) => Movement.moveCostTo(char, v2, tileData, terrainData, vegData, physicalData).map(_.asFloat),
		(v1, v2)  => v1.distance(v2) + v1.q * 0.00001f
	)

	val mousePositionWatcher = Watcher((pixelToHex(Mouse.currentPosition), graphicsWorldView.currentTime))

	controlEvents.listen {
		case MousePressEvent(button, pos, modifiers) => {
			if (!graphics[AnimationData].animationsInProgress) {
				implicit val view = graphicsWorldView

				for (action <- graphics[Selection].selectedPossibleActions) {
					action pmatch {
						case SwitchActiveCharacter(character) => graphics[Selection].selectedCharacters = Set(character)
					}

					Action.performAction(action)
				}
			}
		}
		case KeyPressEvent(key, modifiers, _) => {
			key pmatch {
				case GLFW.GLFW_KEY_E => Turns.transitionFactionTurn()
				case GLFW.GLFW_KEY_ESCAPE => graphics[Selection].selectedCharacters = Set()
			}
		}
	}


	override protected def initialize(): Unit = {
		val desktop = control[WindowingData].desktop

		desktop[DrawingData].withData { o =>
			o.drawAsForegroundBorder = true
		}

		val sideBar = new SideBar(desktop)(graphicsWorldView, controlEngine.graphicsWorld)
	}

	override protected def updateSelf(dt: UnitOfTime): Unit = {
		implicit val view = graphicsWorldView

		// todo: also give a spot to put errors (i.e. display a little X or something when no path can be found)
		var newPossibleActions : List[GameAction] = Nil
		if (!graphics[AnimationData].animationsInProgress) {
			mousePositionWatcher.changedValue match {
				case None =>
					// if nothing has changed, just keep with what we had
					newPossibleActions = graphics[Selection].selectedPossibleActions
				case Some((newPos, currentTime)) =>
					val charOnTile = Tiles.characterOnTile(newPos)
					if (charOnTile.exists(c => Characters.isPlayerCharacter(c))) {
						newPossibleActions = List(SwitchActiveCharacter(charOnTile.get))
					} else {
						for (selChar <- graphics[Selection].selectedCharacters) {
							val selTileEnt = Tiles.tileAt(newPos)
							val selTile = selTileEnt[Tile]

							charOnTile match {
								case None =>
									val curPos = selChar[Physical].position
									pathfinder.findPathTo(selChar, curPos, newPos) pmatch {
										case Some(path) => {
											val subPath = Movement.subPath(selChar, path, selChar[CharacterInfo].movePoints.currentValue)
											if (subPath.steps.size < 2) {
												newPossibleActions = Nil
											} else {
												newPossibleActions = List(MoveAction(selChar, subPath))
											}
										}
									}
								case Some(otherChar) =>
									if (Characters.areFriendly(selChar, otherChar)) {
										// do nothing
									} else {
										// TODO: path to first
										val possibleAttacks = Attacks.availableAttacks(selChar)

										possibleAttacks.toList.flatMap { case (w, attacks) => attacks.map(a => w -> a) }.find { case (w,attack) =>
											Attacks.canAttack(AttackAction(selChar, selChar[Physical].position, Set(otherChar), Some(w), attack))
										} match {
											case Some((weapon, attack)) => newPossibleActions = List(AttackAction(selChar, selChar[Physical].position, Set(otherChar), Some(weapon), attack))
											case None => newPossibleActions = Nil
										}
									}
							}
						}
					}
			}
		}
		graphics[Selection].selectedPossibleActions = newPossibleActions
	}

	override def activateSelf(): Unit = {

	}

	override def deactivateSelf(): Unit = {

	}
}
