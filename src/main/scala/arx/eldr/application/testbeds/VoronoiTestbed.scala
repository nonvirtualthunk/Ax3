package arx.eldr.application.testbeds

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.algorithms.DelaunayTriangulation
import arx.core.datastructures.Voronoi
import arx.core.math.Rectf
import arx.core.metrics.Metrics
import arx.core.units.UnitOfTime
import arx.core.vec.ReadVec2f
import arx.core.vec.Vec2f
import arx.engine.advanced.Engine
import arx.engine.control.ControlEngine
import arx.engine.control.components.ControlComponent
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.CanvasGraphicsComponent
import arx.engine.graphics.data.PovData
import arx.engine.graphics.data.TGraphicsData
import arx.engine.simple.Canvas
import arx.graphics.helpers.Color
import arx.graphics.pov.TopDownCamera
import arx.resource.ResourceManager

import scala.util.Random
import scalaxy.loops.rangeExtensions

object VoronoiTestbed extends SuperSimpleTopDownDisplayTestbed(500.0f) {
	val rand = new Random()
	var points = fillList(3000)(_ => Vec2f(rand.nextFloat() * 1000.0f - 500.0f, rand.nextFloat() * 1000.0f - 500.0f))
	var voronoi = Voronoi.fromPoints(points, Rectf(-500.0f, -500.0f, 1000.0f, 1000.0f))

	println("Max neighbors: " + voronoi.neighbors.toCountList.minBy(_._2))

	// lloyd relaxation
	for (i <- 0 until 3) {
		for (reg <- voronoi.regions if reg.edges.nonEmpty) {
			val center = reg.edges.map(e => e.start + e.end).reduce(_ + _) / (reg.edges.size.toFloat * 2.0f)
			points(reg.originatingIndex).x = center.x
			points(reg.originatingIndex).y = center.y
		}

		voronoi = Voronoi.fromPoints(points, Rectf(-500.0f, -500.0f, 1000.0f, 1000.0f))
	}

	override def draw(canvas: Canvas): Unit = {
		for (p <- points) {
			canvas.drawQuad(p, Vec2f(3f, 3f), Color.White, ResourceManager.image("default/blank.png"))
		}

		for (r1 <- voronoi.regions) {
			for (e0 <- r1.edges) {
				val r2 = e0.otherRegion(r1)
//				r1.edges.find(e => e.otherRegion(r1).edges)
//				r2.edges.find(e => e != e0 && e.otherRegion(r2) == r1)
				val otherR2Edges = r2.edges.filterNot(e => e eq e0)
				val otherRegions = otherR2Edges.flatMap(e => e.regionA :: e.regionB :: Nil)
				otherRegions.find(r => r.edges.exists(e => e.otherRegion(r) eq r1)) match {
					case Some(r3) =>
						val p1 = points(r1.originatingIndex)
						val p2 = points(r2.originatingIndex)
						val p3 = points(r3.originatingIndex)

						canvas.line(p1,p2,1.0f,Color.Blue)
						canvas.line(p2,p3,1.0f,Color.Blue)
						canvas.line(p3,p1,1.0f,Color.Blue)
					case None => // do nothing?
				}
			}
		}

//		for ((p,i) <- voronoi.points.zipWithIndex) {
//			canvas.drawQuad(p, Vec2f(2f,2f), Color.Green, ResourceManager.blankImage)
//			val neighborIndices = voronoi.neighbors.get(i).distinct
//			if (neighborIndices.size == 3) {
//				for (metaJ <- 0 until 3 optimized) {
//					val p1 = voronoi.points(neighborIndices(metaJ))
//					val p2 = voronoi.points(neighborIndices((metaJ+1)%3))
//
//					canvas.line(p,p1, 1.0f, Color.Red)
//					canvas.line(p,p2, 1.0f, Color.Red)
//					canvas.line(p1,p2, 1.0f, Color.Red)
//				}
//			}
//			for (j <- neighborIndices.toStream) {
//				val otherPoint = voronoi.points(j)
////				val midpoint = (p + otherPoint) * 0.5f
//				canvas.line(p, otherPoint, 1.0f, Color.Red)
//			}
//		}

		for (reg <- voronoi.regions) {
			for (edge <- reg.edges) {
				canvas.line(edge.start, edge.end, 1.0f, Color.Red)

			}
		}
	}
}


object DelaunayTriangulationTestbed extends SuperSimpleTopDownDisplayTestbed(1000.0f) {
	val rand = new Random()
	val points = fillVector(5000)(_ => Vec2f(rand.nextFloat() * 2000.0f - 1000.0f, rand.nextFloat() * 2000.0f - 1000.0f))
	val triangulation = DelaunayTriangulation.triangulate(points)

	override def draw(canvas: Canvas): Unit = {
		for (p <- points) {
			canvas.drawQuad(p, Vec2f(3f, 3f), Color.White, ResourceManager.image("default/blank.png"))
		}
		for (tri <- triangulation) {
			for (i <- 0 until 3 optimized) {
				val p0 = points(tri.p(i))
				val p1 = points(tri.p((i+1) % 3))
				canvas.line(p0, p1, 1.0f, Color.Red)
			}
		}
	}
}

object DelaunayTriangulationBenchmark {
	def main(args : Array[String]): Unit = {
		val rand = new Random()
		val points = fillVector(50000)(_ => ReadVec2f(rand.nextFloat() * 1000.0f, rand.nextFloat() * 1000.0f))

		val timer = Metrics.timer("delaunay test")
		for (i <- 0 until 2 optimized) {
			timer.timeStmt {
				val del = DelaunayTriangulation.triangulate(points)
				println(s"Triangles: ${del.size}")
			}
		}

		Metrics.prettyPrintTimer(timer)
	}
}

abstract class SuperSimpleTopDownDisplayTestbed(scale: Float) extends Engine {
	protected val self = this

	override def setUpEngine(): Unit = {
		val pov = new TopDownCamera(scale * 2.0f)
		pov.viewDistance = scale * 2.1f

		graphicsEngine.graphicsWorld.aux[PovData].pov = pov
		graphicsEngine.graphicsWorld.aux[SuperSimpleDisplayData].drawFunc = this.draw

		graphicsEngine.addComponent[SuperSimpleDisplayComponent]

	}

	def draw(canvas: Canvas): Unit
}

class PovControlComponent(ce : ControlEngine) extends ControlComponent(ce) {
	override protected def update(dt: UnitOfTime): Unit = {
		graphics[PovData].pov.update(dt)
	}
}

class SuperSimpleDisplayData extends TGraphicsData {
	var drawFunc : (Canvas) => Unit = (c) => {}
}

class SuperSimpleDisplayComponent(ge: GraphicsEngine) extends CanvasGraphicsComponent(ge) {
	canvas.useHighCapacity(true)

	override def draw(canvas: Canvas): Unit = {
		graphics[SuperSimpleDisplayData].drawFunc(canvas)
	}
}