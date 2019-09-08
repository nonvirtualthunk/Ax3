package arx.tyche.game.logic

import java.util

import arx.core.datastructures.MultiMap
import arx.core.vec.{ReadVec2f, ReadVec3f, Vec2f, Vec3f}
import arx.engine.entity.GameEntity
import arx.engine.world.World
import arx.tyche.core.GlobeCoord
import arx.tyche.game.data.TerrainType.Grassland
import arx.tyche.game.data.{Globe, Patch}
import io.github.jdiemke.triangulation.{DelaunayTriangulator, Vector2D}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scalaxy.loops._

object GlobeGenerator {
	def toArx(v : Vector2D) : ReadVec2f = Vec2f(v.x.toFloat, v.y.toFloat)
	def fromArx(v : ReadVec2f) : Vector2D = new Vector2D(v.x, v.y)

	def project(v : ReadVec2f) : ReadVec3f = {
		val denom = 1.0f + v.x * v.x + v.y * v.y
		Vec3f((v.x * 2.0f) / denom, (v.y * 2.0f) / denom, (-1.0f + v.x * v.x + v.y * v.y) / denom)
	}

	/**
	  * Computes the centroid of the triangle (a,b,c) on the surface of a sphere, then projects that centroid itself onto the surface of the sphere
	  */
	def centroid(a : ReadVec3f, b : ReadVec3f, c : ReadVec3f) : ReadVec3f = {
		val ac = c - a
		val ab = b - a

		val abXac = ab.cross(ac)

		// this is the vector from a TO the circumsphere center
		val toCircumsphereCenter = (abXac.cross(ab) * ac.lengthSquared + ac.cross(abXac) * ab.lengthSquared) / (2.0f * abXac.lengthSquared)

		// The 3 space coords of the circumsphere center then:
		val ccs = a + toCircumsphereCenter
		ccs.normalizeSafe
	}

	case class Tri(ai : Int, bi : Int, ci : Int) {
		def indices = List(ai,bi,ci)
	}

	def generate(world : World, radius : Float): Unit = {
		val globe = world[Globe]

		val cant = 300
		val scale = 25.0f
		val offset = 2.0f / cant.toFloat
		val increment = Math.PI * (3 - Math.sqrt(5))

		val angle = 0.0

		val rawPoints = for (i <- 0 until cant) yield {
			val y = (i * offset - 1) + (offset / 2)
			val r = Math.sqrt(1 - Math.pow(y, 2))
			val a = ((i + 1) % cant) * increment + angle
			val x = (Math.cos(a) * r).toFloat
			val z = (Math.sin(a) * r).toFloat

			Vec3f(x,y,z)
//			val gc = GlobeCoord.fromPointOnSphere(x * scale,y * scale,z * scale)
		}

		val projected = rawPoints.map(p => Vec2f(p.x / (1.0f - p.z), p.y / (1.0f - p.z)))

		val pointList = new util.ArrayList[Vector2D]()
		projected.foreach(p => pointList.add(fromArx(p)))

		val indicesByPoint = pointList.asScala.zipWithIndex.toMap

		val triangulator = new DelaunayTriangulator(pointList)
		triangulator.triangulate()
		val triangles = triangulator.getTriangles.asScala.map(t => Tri(indicesByPoint(t.a), indicesByPoint(t.b), indicesByPoint(t.c)))

		globe.delaunyTris = triangles.map(t => Vector(GlobeCoord.fromPointOnSphere(rawPoints(t.ai)), GlobeCoord.fromPointOnSphere(rawPoints(t.bi)), GlobeCoord.fromPointOnSphere(rawPoints(t.ci)))).toVector

		val trianglesTouchingIndex = MultiMap.empty[Int, Tri]
		triangles.foreach(t => t.indices.foreach(i => trianglesTouchingIndex.add(i, t)))

		// Fill in the missing triangles
		val indicesWithMissingTris = trianglesTouchingIndex.toList.filter(t => t._2.size == 4)
		val (missingAi, missingATs) = indicesWithMissingTris(0)
		val (missingBi, missingBTs) = indicesWithMissingTris(1)

		val sharedIndices = missingATs.flatMap(t => t.indices).toSet.intersect(missingBTs.flatMap(t => t.indices).toSet)

		for (sharedIndex <- sharedIndices) {
			val newTri = Tri(missingAi, missingBi, sharedIndex)
			globe.delaunyTris +:= newTri.indices.map(vi => GlobeCoord.fromPointOnSphere(rawPoints(vi))).toVector

			newTri.indices.foreach(idx => trianglesTouchingIndex.add(idx, newTri))
		}

		var neighborSets = Map[Int,Set[Int]]()
		val patches = for (i <- rawPoints.indices) yield {
			val relevantTris = trianglesTouchingIndex.get(i)
			val center = rawPoints(i)
			val points = relevantTris.map(t => {
				val a = rawPoints(t.ai)
				val b = rawPoints(t.bi)
				val c = rawPoints(t.ci)

//				centroid(a,b,c)
				((a + b + c) / 3.0f).normalizeSafe
			}).sortWith((a,b) => (a - center).cross(b - center).dot(center.normalizeSafe) >= 0.0f)


			val neighborSet = relevantTris.flatMap(t => t.indices).filterNot(idx => idx == i).toSet
			neighborSets += i -> neighborSet

			val patchEnt = new GameEntity()
			world.addEntities(patchEnt)

			val patch = patchEnt[Patch]
			patch.vertices = points.map(GlobeCoord.fromPointOnSphere).toList
			patch.center = GlobeCoord.fromPointOnSphere(center)
			patch.entity = patchEnt

			if (i < 20) {
				patchEnt[Patch].terrainType = Grassland
			}
			patchEnt
		}

		for ((i, neighborIndices) <- neighborSets) {
			patches(i)[Patch].neighbors = neighborIndices.map(idx => patches(idx)).toList
		}


//		val patches = triangulator.getTriangles.asScala.map(t => {
//			val reprojectedPoints = Seq(t.a, t.b, t.c).map(toArx).map(project)
//			val patch = new Patch
//			patch.vertices = reprojectedPoints.map(GlobeCoord.fromPointOnSphere).toList
//			patch.center = GlobeCoord.fromPointOnSphere((reprojectedPoints.reduceLeft(_ + _) / 3.0f).normalizeSafe)
//			patch
//		})

		globe.patches = patches.toVector
		globe.patchData = globe.patches.map(p => p[Patch])
	}
}
