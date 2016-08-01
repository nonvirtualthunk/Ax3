package arx.slime.game.testbed.control

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.control.ControlEngine
import arx.control.ControlMode
import arx.core.units.UnitOfTime
import arx.gui2.Widget

import scalaxy.loops._
import arx.core.ImplicitModdable._
import arx.core.Moddable
import arx.core.vec.Vec2f
import arx.core.vec.Vec3f
import arx.engine.EngineCore
import arx.engine.control.event.Event.MousePressEvent
import arx.engine.entity.GameEntity
import arx.engine.entity.TGameEntity
import arx.graphics.GL
import arx.graphics.Image
import arx.gui2.widgets.ImageDisplayWidget
import arx.gui2.widgets.TextDisplayWidget
import arx.resource.ResourceManager
import arx.slime.game.data.CreatureData
import arx.slime.game.data.PhysicalData

class SlimeControlMode(ce : ControlEngine) extends ControlMode(ce) {
	var selectedEntity : TGameEntity = GameEntity.Sentinel
	lazy val infoWidget = createInfoWidget()

	override def activate(): Unit = {
		Noto.info("Activating control mode: slime")

		addWidget(infoWidget)
	}

	override def update(dt: UnitOfTime): Unit = {

	}

	override def deactivate(): Unit = {
		Noto.info("Deactivating control mode: slime")

		removeWidget(infoWidget)
	}

	onEvent {
		case MousePressEvent(button,pos,_) =>
			val pov = controlEngine.graphicsEngine.pov
			val effMousePos = Vec2f(pos.x * EngineCore.pixelScaleFactor, EngineCore.pixelHeight - pos.y * EngineCore.pixelScaleFactor - 1)
			val unprojectedA = GL.unproject(Vec3f(effMousePos,0.0f), pov.modelviewMatrix, pov.projectionMatrix, GL.viewport)
			val unprojectedB = GL.unproject(Vec3f(effMousePos,1.0f), pov.modelviewMatrix, pov.projectionMatrix, GL.viewport)
			val delta = unprojectedB - unprojectedA
			val eff = unprojectedA + delta * (-unprojectedA.z / delta.z)

			Noto.info("Unprojected: " + unprojectedA + " -> " + unprojectedB + " : " + eff)

			val tmp = world.auxDataQuery[PhysicalData].results.map(e => e[PhysicalData])
			selectedEntity = world.auxDataQuery[PhysicalData].find(e => distance(e[PhysicalData].position.toCartesian,eff.xy) < 1.0f).getOrElse(GameEntity.Sentinel)
	}


	def createInfoWidget() = {
		val w = createWidget()
		w.width = 30.0f
		w.height = 50.0f
		w.x = 10.0f
		w.y = 10.0f

		val img = new ImageDisplayWidget(w)
		img.width = 10.0f
		img.height = 10.0f
		img.x = 1.0f
		img.y = 1.0f
		img.image = () => selectedEntity.isSentinel match {
			case true => Image.Sentinel
			case false => selectedEntity.archetype.map(arch => {
				val base = arch.name.stripWhitespace.toCamelCase
				val eff = base.substring(0,1).toLowerCase() + base.substring(1)

				ResourceManager.image("slime/entities/" + eff + "/image.png")
			}).getOrElse(Image.Sentinel)
		}

		var lastY = img.y + img.height + 5.0f
		def createStatDisplay[T](statName : String, valueF : (CreatureData) => Moddable[T]): Unit = {
			val text = new TextDisplayWidget(() => selectedEntity.isSentinel match {
				case true => ""
				case false => statName  + ": " + valueF(selectedEntity[CreatureData]).resolve()
			},w)
			text.backgroundImage = Image.Sentinel
			text.x = 1.0f
			text.y = lastY
			text.matchTextDimensions()
			text.fontSize = 3.0f
			lastY = text.y + text.height
		}

		createStatDisplay("HP",_.hp)
		createStatDisplay("Armor",_.armor)
		createStatDisplay("Strength",_.strength)
		createStatDisplay("Land Move",_.landMovement)
		createStatDisplay("Water Move",_.waterMovement)
		createStatDisplay("Land Surival",_.landSurvival)
		createStatDisplay("Water Survival",_.waterSurvival)

		w.showingCondition = () => selectedEntity.notSentinel

		w
	}
}
