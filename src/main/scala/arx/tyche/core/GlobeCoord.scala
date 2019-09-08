package arx.tyche.core

import arx.Prelude._
import arx.core.vec.{ReadVec3f, Vec3f}
import org.joml.{AxisAngle4f, Quaternionf, Vector3f}
import arx.Prelude.{cosf, float2RicherFloat, sinf}
import arx.application.Noto
import arx.core.math.Interpolation
import arx.core.traits.TArxNumeric

case class GlobeCoord protected[core] (rotation : Quaternionf, z : Float) extends TArxNumeric[GlobeCoord]{
	if (rotation.x.isNaN) {
		Noto.info("waT")
	}

	def plusZ(dz: Float): GlobeCoord = new GlobeCoord(rotation, z + dz)
	def withZ(newZ : Float) : GlobeCoord = new GlobeCoord(rotation, newZ)
	def withRotation(rot : Quaternionf) : GlobeCoord = new GlobeCoord(rot, z)

	def zero = new GlobeCoord(new Quaternionf(), 0.0f)

	def asCartesian : ReadVec3f = {
		val tmp = rotation.transform(new Vector3f(z,0,0))
		Vec3f(tmp.x, tmp.y, tmp.z)
	}


	override def +(other: GlobeCoord): GlobeCoord = {
		new GlobeCoord(rotation.mul(other.rotation, new Quaternionf), z + other.z)
	}
	override def -(other: GlobeCoord): GlobeCoord = {
		new GlobeCoord(rotation.mul(other.rotation.invert(new Quaternionf), new Quaternionf), z - other.z)
	}

	override def *(other: Float): GlobeCoord = {
		new GlobeCoord(GlobeCoord.quaternionMulf(rotation, other), z * other)
	}

	def rotated(yaw : Float, pitch : Float) : GlobeCoord = {

		new GlobeCoord(GlobeCoord.quaternionFromEuler(yaw, pitch).mul(rotation), z)
	}

	def slerp (other : GlobeCoord, alpha : Float) : GlobeCoord = {
		val newRot = new Quaternionf(this.rotation)
		newRot.slerp(other.rotation, alpha, newRot)
		new GlobeCoord(newRot.normalize(), this.z + (other.z - this.z) * alpha)
	}

	def angularDistanceTo(other : GlobeCoord) : Float = {
		acosf((2.0f * this.rotation.dot(other.rotation) - 1.0f).clamp(-1.0f,1.0f))
	}

	def moveTowards(target : GlobeCoord, distance : Float) : GlobeCoord = {
		val totalDist = angularDistanceTo(target)
		if (totalDist < 0.01f) {
			this
		} else {
//			val diff = target.rotation.difference(this.rotation, new Quaternionf).normalize().scale(Math.min(distance, totalDist))
//			//		val newRotation = rotation.slerp(target.rotation, (distance / totalDist.max(0.00001f)).clamp(0.01f, 1.0f), new Quaternionf).normalize()
//			//		val newRotation = rotation.slerp(rotation.mul(diff.scale(distance)), 1.0f, new Quaternionf)
//			val newRotation = rotation.mul(diff, new Quaternionf)
//			new GlobeCoord(newRotation, this.z)


//			val axisA = Vec3f(rotation.x, rotation.y, rotation.z)
//			val axisB = Vec3f(target.rotation.x, target.rotation.y, target.rotation.z)

//			val vecA = rotation.transform(1.0f,0.0f,0.0f, new Vector3f)
//			val vecB = target.rotation.transform(1.0f,0.0f,0.0f, new Vector3f)
//
//			val angleBetween = vecA.dot(vecB).abs
//			if (angleBetween < 0.001f) {
//				this
//			} else {
////				new GlobeCoord(rotation.slerp(target.rotation, distance / angleBetween, new Quaternionf()), this.z)
////				val rotateAround = vecA.cross(vecB, new Vector3f)
////
////				new GlobeCoord(rotation.rotateAxis(Math.min(distance, totalDist), rotateAround, new Quaternionf), this.z)
//			}

			new GlobeCoord(GlobeCoord.modifiedSlerp(this.rotation, target.rotation, distance), this.z)
		}
	}

	def interpolate(other : GlobeCoord) : Interpolation[GlobeCoord] = {
		new Interpolation[GlobeCoord] {
			override def interpolate(pcnt: Float): GlobeCoord = {
				val z = GlobeCoord.this.z + (other.z - GlobeCoord.this.z) * pcnt
				val rot = GlobeCoord.this.rotation.slerp(other.rotation, pcnt, new Quaternionf)
				new GlobeCoord(rot, z)
			}
		}
	}

	def tangents : (ReadVec3f, ReadVec3f) = {
		val v = new Vector3f()
		rotation.transform(new Vector3f(0.0f, 1.0f, 0.0f), v)
		val t1 = Vec3f(v.x,v.y,v.z)
		rotation.transform(new Vector3f(0.0f, 0.0f, 1.0f), v)
		val t2 = Vec3f(v.x,v.y,v.z)
		(t1,t2)
	}
}


object GlobeCoord {

	def apply() : GlobeCoord = new GlobeCoord(new Quaternionf(), 0.0f)

	def fromEuler(yaw : Float, pitch : Float, z : Float) = new GlobeCoord(GlobeCoord.quaternionFromEuler(yaw, pitch), z)

	def fromPointOnSphere(v : ReadVec3f) : GlobeCoord = {
		fromPointOnSphere(v.x, v.y, v.z)
	}
	def fromPointOnSphere(x : Float, y : Float, z : Float) : GlobeCoord = {
		val v = Vec3f(x,y,z)
		val vn = v.normalizeSafe
		val l = v.lengthSafe

		val r = Math.sqrt(x*x + y*y + z*z)
		val t = Math.atan2(y,x)
		val p = Math.acos(z/r)

		new GlobeCoord(new Quaternionf().rotateTo(1.0f,0.0f,0.0f, vn.x, vn.y, vn.z), l)
//		new GlobeCoord(new Quaternionf().rotateXYZ(0.0f, t.toFloat,p.toFloat), l)
	}

	def atZ(z : Float) : GlobeCoord = new GlobeCoord(new Quaternionf(), z)

	def quaternionMulf(quat : Quaternionf, f : Float) : Quaternionf = {
		new Quaternionf(quat.x * f, quat.y * f, quat.z * f, quat.w * f)
	}

	def quaternionFromEuler(yaw : Float, pitch : Float) = {
//		val cy: Float = cosf(yaw * 0.5f)
//		val sy: Float = sinf(yaw * 0.5f)
//		val cp: Float = cosf(pitch * 0.5f)
//		val sp: Float = sinf(pitch * 0.5f)
//		val cr: Float = 1.0f
//		val sr: Float = 0.0f
//		new Quaternionf(cy * cp * sr - sy * sp * cr, sy * cp * sr + cy * sp * cr, sy * cp * cr - cy * sp * sr, cy * cp * cr + sy * sp * sr)
		val tmp = new Quaternionf
		tmp.rotateXYZ(0.0f, pitch, yaw)
	}

	private def modifiedSlerp(source : Quaternionf, target : Quaternionf, deltaAngle : Float) : Quaternionf = {
		val v0 = source.normalize(new Quaternionf())
		val v1 = target.normalize(new Quaternionf())

		var dot = source.dot(target)

		if (dot < 0.0f) {
			v1.invert()
			dot = -dot
		}

		val dotThreshold = 0.99999f
		if (dot > dotThreshold) {
//			val dx = v1.x - v0.x
//			val dy = v1.y - v0.y
//			val dz = v1.z - v0.z
//			val dw = v1.w - v0.w
//
//			new Quaternionf(v0.x + dx * )
			v1
		} else {
			val theta0 = acosf(dot)
			val theta = deltaAngle
			if (theta.abs > theta0.abs) {
				target
			} else {
				val sin_theta = sinf(theta)
				val sin_theta_0 = sinf(theta0)

				val s0 = cosf(theta) - dot * sin_theta / sin_theta_0 // == sin(theta_0 - theta) / sin(theta_0)
				val s1 = sin_theta / sin_theta_0

				v0.x *= s0
				v0.y *= s0
				v0.z *= s0
				v0.w *= s0

				v1.x *= s1
				v1.y *= s1
				v1.z *= s1
				v1.w *= s1

				v0.add(v1).normalize()
			}
		}
	}
}