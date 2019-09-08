package arx.tyche.graphics.data

import arx.core.richer.WrappedFloat
import arx.core.traits.TArxNumeric
import arx.core.vec.{ReadVec2f, ReadVec4f}
import arx.tyche.core.GlobeCoord
import arx.Prelude._
import arx.core.math.Interpolation
import arx.core.units.UnitOfTime
import arx.engine.graphics.data.TGraphicsData
import arx.graphics.{Image, TToImage}

import scala.language.implicitConversions

class Particles extends TGraphicsData {
	var particlesBySource : Map[AnyRef, List[Particle]] = Map()

	def addParticle(src : AnyRef, p : Particle, time : UnitOfTime) : Unit = {
		p.startTime = time
		particlesBySource += src -> (p :: particlesBySource.getOrElse(src, Nil))
	}

	def withSource(src : AnyRef) = particlesBySource.getOrElse(src, Nil)

	def cullParticles(currentTime : UnitOfTime) : Unit = {
		particlesBySource = particlesBySource.mapValues(ps => ps.filter(p => p.startTime + p.lifetime > currentTime))
	}
}


case class IPD[X <: TArxNumeric[X]](var value : X, var velocity : X, var acceleration : X) {
	def advance(dt : Float): Unit = {
		value = value + velocity
		velocity = velocity + acceleration
	}
}
object IPD {
	def apply[X <: TArxNumeric[X]](value : X) : IPD[X] = IPD[X](value, value.zero, value.zero)
	def apply[X <: TArxNumeric[X]](value : X, velocity : X) : IPD[X] = IPD[X](value, velocity, value.zero)
	implicit def fromRaw[X <: TArxNumeric[X]](value : X) : IPD[X] = IPD[X](value, value.zero, value.zero)
}



case class Particle(private val positionInterp : Interpolation[GlobeCoord],
						  private val dimensionsInterp : Interpolation[ReadVec2f],
						  private val colorInterp : Interpolation[ReadVec4f],
						  private val toTexture : TToImage,
						  lifetime : UnitOfTime,
						  private val lightPcntInterp : Interpolation[Float] = 0.0f) {
	var startTime = 0.seconds
	val texture = toTexture.image

	def position(curTime : UnitOfTime) = positionInterp.interpolate(startTime, lifetime, curTime)
	def dimensions(curTime : UnitOfTime) = dimensionsInterp.interpolate(startTime, lifetime, curTime)
	def color(curTime : UnitOfTime) = colorInterp.interpolate(startTime, lifetime, curTime)
	def lightPcnt(curTime : UnitOfTime) = lightPcntInterp.interpolate(startTime, lifetime, curTime)
}