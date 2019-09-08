package arx.samvival.graphics.animation

import arx.Prelude._
import arx.core.introspection.Field
import arx.core.introspection.FieldOperations.SetTo
import arx.core.math.{Interpolation, Rectf}
import arx.core.units.UnitOfTime
import arx.core.vec.{Cardinals, ReadVec2f}
import arx.core.vec.coordinates.{CartVec, CartVec3}
import arx.engine.control.components.windowing.widgets.FontWrapper
import arx.engine.data.TAuxData
import arx.engine.graphics.data.TGraphicsData
import arx.engine.lworld._
import arx.graphics.Image
import arx.graphics.helpers.{HSBA, RichText}
import arx.graphics.text.HorizontalTextAlignment
import arx.graphics.text.HorizontalTextAlignment.Centered
import arx.samvival.graphics.components.Layers

import scala.reflect.ClassTag

class AnimationData extends TGraphicsData {
	// can we figure out a way to do animationSpeed modifiers without having to rely on deltas and just be
	// able to use absolute times? Absolute times seem to do a lot better for the reliable smoothness of movement

	// I think the answer is that the animation clock is always based on wall time, and animations record their
	// start based on wall time, and we determine duration by using the animation speed multiplier
//	var animationClock : UnitOfTime = 0.seconds

	protected[animation] var animationSpeed : Float = 1.0f
	protected[animation] var ongoingAnimations = Vector[Animation]()
	protected[animation] var animationQueue = Vector[Animation]()
	protected[animation] var lastAnimated : GameEventClock = GameEventClock(0)
	protected[animation] var activeAnimationsElements = Vector[AnimationElement]()

	def animationsInProgress : Boolean = animationQueue.nonEmpty

	def registerAnimation(animation : Animation): Unit = {
		animationQueue :+= animation
	}

	def registerAnimationElement(animation : AnimationElement): Unit = {
		activeAnimationsElements :+= animation
	}
}

sealed class StartPoint
object StartPoint {
	case class Pcnt(pcnt: Float) extends StartPoint
	case class Offset(offset: UnitOfTime) extends StartPoint
	def Immediate = Offset(0.seconds)
}

sealed class AnimationDuration
object AnimationDuration {
	case class Pcnt(pcnt: Float) extends AnimationDuration
	case class Time(time : UnitOfTime) extends AnimationDuration
	def Full = Pcnt(1.0f)
}

case class AnimationElementWrapper(startPoint : StartPoint, duration : AnimationDuration, element : AnimationElement)

case class Animation(protected[animation] val duration : UnitOfTime, protected[animation] val blockingDuration : UnitOfTime) {
	protected[animation] var startTime: Option[UnitOfTime] = None
	var elements = List[AnimationElementWrapper]()

	def withElement(element : AnimationElement) = {
		elements ::= AnimationElementWrapper(StartPoint.Immediate, AnimationDuration.Full, element)
		this
	}
	def withElement(startPcnt : Float, endPcnt : Float, element : AnimationElement) = {
		elements ::= AnimationElementWrapper(StartPoint.Pcnt(startPcnt), AnimationDuration.Pcnt(endPcnt - startPcnt), element)
		this
	}
	def withElement(startPcnt : Float, duration : AnimationDuration, element : AnimationElement) = {
		elements ::= AnimationElementWrapper(StartPoint.Pcnt(startPcnt), duration, element)
		this
	}
}

sealed abstract class AnimationElement {
	var startTime: UnitOfTime = 0.seconds
	var duration: UnitOfTime = 10.seconds

	def endTime(animationSpeed : Float) = startTime + duration  / animationSpeed
}

trait DrawableAnimationElement extends AnimationElement {
	def position(pcnt: Float): CartVec3

	def size(pcnt: Float): CartVec
}


case class ImageAnimationElement(position: Interpolation[CartVec3],
											size: Interpolation[CartVec],
											image: Interpolation[Image],
											color: Interpolation[HSBA],
											layer : Int = Layers.Effects)
	extends AnimationElement with DrawableAnimationElement {

	override def position(pcnt: Float): CartVec3 = position.interpolate(pcnt)

	override def size(pcnt: Float): CartVec = size.interpolate(pcnt)
}

case class TextAnimationElement(position : Interpolation[CartVec3],
										  fontHexSize : Interpolation[Float],
										  color : Interpolation[HSBA],
										  text : Interpolation[RichText],
										  layer : Int = Layers.Effects,
										  textAlignment : HorizontalTextAlignment = Centered,
										  font : Option[String] = None)
extends AnimationElement with DrawableAnimationElement {
	override def position(pcnt: Float): CartVec3 = position.interpolate(pcnt)

	override def size(pcnt: Float): CartVec = {
		val fs = fontHexSize.interpolate(pcnt)
		CartVec(fs, fs)
	}
}

case class PropertyAnimationElement(entity : LEntity, transformation : PropertyTransformation) extends AnimationElement {

}
object PropertyAnimationElement {
	def apply[C <: TAuxData, T](entity : LEntity, field : Field[C, T], interpolation: Interpolation[T])(implicit classTag : ClassTag[C]) : PropertyAnimationElement = {
		PropertyAnimationElement(entity, TypedPropertyTransformation(field, interpolation))
	}

	implicit class AnimateableEntity(val entity : LEntity) {
		def animate[C <: TAuxData, T](field : Field[C, T], interpolation: Interpolation[T])(implicit classTag : ClassTag[C]) : PropertyAnimationElement = {
			PropertyAnimationElement(entity, TypedPropertyTransformation(field, interpolation))
		}
	}
}

trait PropertyTransformation {
	def applyTo(worldView : LWorldView, entity : LEntity, pcnt : Float)
}
case class TypedPropertyTransformation[C <: TAuxData, T](field : Field[C, T], interpolation: Interpolation[T])(implicit val classTag : ClassTag[C]) extends PropertyTransformation {
	override def applyTo(worldView : LWorldView, entity: LEntity, pcnt: Float): Unit = {
		val newValue = interpolation.interpolate(pcnt)
		worldView.applyOverlayModification(Modification[C](entity, new FieldOperationModifier[C,T](field, new SetTo[T](newValue)), None))
	}
}