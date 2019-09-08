package arx.samvival.graphics.animation

import arx.Prelude._
import arx.application.Noto
import arx.core.async.Executor
import arx.core.introspection.ReflectionAssistant
import arx.core.math.Rectf
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.{CartVec, CartVec3}
import arx.engine.EngineCore
import arx.engine.advanced.lenginecomponents.DrawPriority
import arx.engine.advanced.lenginepieces.LGraphicsEngine
import arx.engine.graphics.components.windowing.TextRenderer
import arx.engine.lworld.EventState
import arx.graphics.helpers.HSBA
import arx.graphics.text.{HorizontalTextAlignment, TextLayouter, VerticalTextAlignment}
import arx.resource.ResourceManager
import arx.samvival.game.entities.Fields.Physical
import arx.samvival.game.events.GameEvents.EntityMoved
import arx.samvival.graphics.animation.PropertyAnimationElement.AnimateableEntity
import arx.samvival.graphics.animation.animators.EventAnimator
import arx.samvival.graphics.components.{SVCanvas, SamvivalCanvasGraphicsComponent, SamvivalGraphicsComponent}
import arx.samvival.graphics.data.CullingData

class AnimationGraphicsComponent(val engine : LGraphicsEngine) extends SamvivalCanvasGraphicsComponent(engine) {
	drawOrder = DrawPriority.Final

	var lastRealTime : Option[UnitOfTime] = None

	val animators = ReflectionAssistant.instancesOfSubtypesOf[EventAnimator]


	override protected def initialize(): Unit = {
		Executor.submitAsync(() => ResourceManager.font("pf_ronda_seven", canvas.textureBlock))
	}

	override def draw(canvas: SVCanvas): Unit = {
		val animationData = graphics[AnimationData]
		val cullData = graphics[CullingData]

		val now = curTime()
		val animationSpeed = animationData.animationSpeed
		lastRealTime = Some(now)

		world.clearOverlay()

		createNewAnimationsIfNecessary()

		processAnimationQueue(animationData, now)

		processPreemptiveActiveAnimationElements(animationData, now, animationSpeed)

		processActiveAnimationElements(animationData, cullData, now)
	}

	private def processPreemptiveActiveAnimationElements(animationData: AnimationData, now: UnitOfTime, animationSpeed: Float) = {
		// apply the animations that should take effect pre-emptively, primarily property transformations
		for (animation <- animationData.activeAnimationsElements) {
			val pcnt = (now - animation.startTime).inSeconds / (animation.duration.inSeconds * animationSpeed)
			if (pcnt >= 0.0f && pcnt <= 1.0f) {
				animation match {
					case PropertyAnimationElement(entity, transformation) => {
						transformation.applyTo(graphicsEngine.view, entity, pcnt)
					}
					case _ => {
						// do nothing
					}
				}
			}
		}
	}

	private def processActiveAnimationElements(animationData: AnimationData, cullData: CullingData, now: UnitOfTime) = {
		var toRemove = Set[AnimationElement]()
		for (animation <- animationData.activeAnimationsElements) {
			val pcnt = (now - animation.startTime).inSeconds / animation.duration.inSeconds
			if (pcnt >= 0.0f && pcnt <= 1.0f) {
				val shouldDraw = animation match {
					case anim: DrawableAnimationElement => {
						val pos = anim.position(pcnt)
						cullData.hexesInView.contains(pos.asAxialVec)
					}
					case _ => false
				}

				if (shouldDraw) {
					animation match {
						case ImageAnimationElement(position, size, image, color, layer) => {
							val curPos = position.interpolate(pcnt)
							val curSize = size.interpolate(pcnt)

							canvas.quad(curPos, layer)
								.withDimensions(curSize)
								.withColor(color.interpolate(pcnt))
								.withLightColor(1, 1, 1)
								.withVisionPcnt(1.0f)
								.withTexture(image.interpolate(pcnt))
								.draw()
						}
						case TextAnimationElement(position, fontHexSize, color, text, layer, textAlignment, font) => {
							val curPos = position.interpolate(pcnt)
							val curFontHexSize = fontHexSize.interpolate(pcnt)
							val curText = text.interpolate(pcnt)
							val curFont = ResourceManager.font(font.getOrElse("pf_ronda_seven"), canvas.textureBlock)
							val curColor = color.interpolate(pcnt)

							val desiredFontSizePixels = curFontHexSize * SamvivalGraphicsComponent.HexSize.toFloat
							val effectiveFontMultiplier = desiredFontSizePixels / curFont.fontMetrics.lineHeight.toFloat

							val startX = textAlignment match {
								case HorizontalTextAlignment.Left => 0.0f
								case HorizontalTextAlignment.Centered => -1000.0f
								case HorizontalTextAlignment.Right =>
									Noto.warn("Right aligned animation text not yet supported")
									0.0f
							}
							val area = Rectf(startX,0.0f,2000.0f,100.0f)

							val layouter = TextLayouter(curFont, effectiveFontMultiplier)
							for (quad <- TextRenderer.render(layouter, curText, area, textAlignment, VerticalTextAlignment.Bottom)) {
								val baseBuilder = canvas.quad(curPos, layer)
   								.withOffsetPixels(quad.rect.xy)
   								.withColor(HSBA.fromRGBA(curColor.toRGBA * quad.color))
   								.withDimensions(quad.rect.dimensions)
   								.withCentered(false)

								val finalBuilder = quad.texCoords match {
									case Some(tc) => baseBuilder.withTexCoords(tc)
									case None => baseBuilder.withTexture(quad.image)
								}

								finalBuilder.draw()
							}
						}
						case _ => {}
					}
				}
			} else if (pcnt > 1.0f) {
				toRemove += animation
			}
		}

		if (toRemove.nonEmpty) {
			animationData.activeAnimationsElements = animationData.activeAnimationsElements.filterNot(toRemove.contains)
		}
	}

	def createNewAnimationsIfNecessary(): Unit = {
		val animationData = graphics[AnimationData]
		// while we're behind core time and there aren't any animations in the queue
		while (animationData.animationQueue.isEmpty && engine.worldCore.currentTime > world.currentTime) {
			// if we're animated up through a certain point, but haven't advanced time, advance time
			if (animationData.lastAnimated >= world.currentTime + 1) {
				engine.worldCore.updateViewToTime(world, world.currentTime + 1)
			} // otherwise, if we haven't updated the upcoming event, do so
			else if (animationData.lastAnimated < world.currentTime + 1) {
				engine.worldCore.eventAt(animationData.lastAnimated + 1) match {
					case Some(event) =>
						animators.foreach(animator => {
							animator.createAnimationFor.lift.apply((event.event, event.state)) match {
								case Some(animations) => animations.foreach(animation => graphics[AnimationData] registerAnimation animation)
								case None =>
							}
						})
						animationData.lastAnimated += 1
					case None =>
				}
			} else {
				println("wat")
				return
			}
		}
	}

	def processAnimationQueue(animationData : AnimationData, now : UnitOfTime): Unit = {
		val animationSpeed = animationData.animationSpeed
		val relevantAnimations = animationData.ongoingAnimations ++ animationData.animationQueue.take(1)
		for (animation <- relevantAnimations) {
			animation.startTime = animation.startTime.orElse(Some(now))
			val relativeTime = (now - animation.startTime.getOrElse(now)) * animationSpeed
			val relativePcnt = relativeTime.inSeconds / animation.duration.inSeconds
			val (elementsToAdd, remainingElements) = animation.elements.partition {
				case AnimationElementWrapper(startPoint, _, _) => {
					startPoint match {
						case StartPoint.Pcnt(pcnt) => pcnt <= relativePcnt
						case StartPoint.Offset(offset) => offset <= relativeTime
					}
				}
			}
			elementsToAdd.foreach(e => {
				val newElement = e.element
				newElement.startTime = now
				newElement.duration = e.duration match {
					case AnimationDuration.Pcnt(pcnt) => pcnt * animation.duration
					case AnimationDuration.Time(time) => time
				}
				animationData.registerAnimationElement(newElement)
			})

			animation.elements = remainingElements
		}

		// keep only the ongoing animations that have not passed their duration yet
		animationData.ongoingAnimations = animationData.ongoingAnimations.filter(a => a.startTime.getOrElse(now) + a.duration / animationSpeed > now)

		// split the queue into animations that are still blocking and those that are simply ongoing
		// animations that have no blocking duration are immediately moved into the ongoing vector
		val (pastBlocking, stillBlocking) =
			animationData.animationQueue.partition(a => a.startTime.getOrElse(now) + a.blockingDuration / animationSpeed <= now)

		animationData.ongoingAnimations ++= pastBlocking
		animationData.animationQueue = stillBlocking
	}
}
