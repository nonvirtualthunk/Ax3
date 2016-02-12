package arx.axistential.game.world.generation.generators.meteorological

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/29/13
 * Time: 11:57 AM
 */

import arx.Prelude._
import arx.axistential.game.data.world.TopologicalData
import arx.axistential.game.data.world.WindData
import arx.axistential.game.world.generation.WorldGenerator.Phase
import arx.axistential.game.world.generation.WorldGenerator.Stage
import arx.axistential.game.world.generation.Progress
import arx.axistential.game.world.generation.TWorldGenerator
import arx.axistential.game.world.generation.WorldGenerator
import arx.core.gen.ArxGenerators._
import arx.core.gen.SimplexNoise
import arx.core.vec.ReadVec2i
import arx.core.vec.Vec2i
import arx.tmp.game.logic.datastructures.GridCell2D
import arx.tmp.game.logic.world.SpatialRegion

import scalaxy.loops._

class WindWorldGenerator extends TWorldGenerator {

	 def findOutliers(topological: TopologicalData, R: SpatialRegion) = {
		 var sum_x1 = 0L
		 var sum_x2 = 0L
		 val n = R.dimensions.x * R.dimensions.y
		 topological.heightmap.allInRegionInclusive(R).foreach ( cell => {
			 for ( x <- 0 until GridCell2D.dimension optimized ; y <- 0 until GridCell2D.dimension optimized ) {
				 val h = cell(x,y)
				 sum_x1 += h
				 sum_x2 += h*h
			 }
		 } )
		 val mean = sum_x1 / n.toDouble
		 val stdDev = math.sqrt((sum_x2 / n.toDouble) - mean*mean)

		 var outliers = Set[ReadVec2i]()
		 val cutoff = mean + stdDev * 2.6
		 topological.heightmap.allInRegionInclusive(R).foreach ( cell => {
			 for ( x <- 0 until GridCell2D.dimension optimized ; y <- 0 until GridCell2D.dimension optimized ) {
				 if ( cell(x,y) > cutoff ) {
					 outliers += Vec2i(cell.x + x,cell.y + y)
				 }
			 }
		 } )

		 outliers -> cutoff
	 }

	 /**
	  * Perform this component's generation, storing the results in the provided world.
 *
	  * @return any new generators that should be used from this point on (i.e. a new geological feature, building, etc)
	  */
	 def generate(world: World, stage: Stage, phase: Phase, progress: Progress): List[TWorldGenerator] = {
		 val WD = world.aux[WindData]
		 val topological = world.aux[TopologicalData]
		 val R = world.worldRegionAsSpatialRegion

		 if ( stage == WorldGenerator.Heightmap ) {
			 if ( phase == WorldGenerator.Meteorological ) {
				 val windSimplex = new SimplexNoise(simplexGenerator.seed + 142)
				 val turbulenceSimplex = new SimplexNoise(simplexGenerator.seed + 143)
				 val prevailingWindGenerator = Scale(0.0f,0.0f,0.001f) >> Simplex(windSimplex) >> Mult(pi * 4.0f)
				 val adjustmentGenerator = /*Turbulence(Scale(0.1f,0.1f,0.1f) >> Simplex(turbulenceSimplex)) >> */Scale(0.01f,0.01f,0.05f) >> Simplex(windSimplex) >> Mult(pi * 0.2f)

				 WD.directionDynamicGenerator = adjustmentGenerator
				 WD.prevailingWindGenerator = prevailingWindGenerator

				 var (outliers,cutoff) = findOutliers(topological,R)

				 val dv2 = dirvec.filter(_.z == 0)
				 var peakPoints = Set[ReadVec2i]()
				 while ( outliers.nonEmpty ) {
					 val base = outliers.head
					 outliers -= base
					 var tmp = base
					 var next = tmp
					 var maxHeight = topological.heightmap(tmp)


					 var continue = true
					 while ( continue ) {
						 tmp = next
						 continue = false
						 for ( q <- 0 until 4 optimized ) {
							 val ax = tmp.x + dv2(q).x
							 val ay = tmp.y + dv2(q).y

							 val ah = topological.heightmap(ax,ay)
							 if ( ah > maxHeight ) {
								 val v = Vec2i(ax,ay)
								 if ( outliers.contains(v) ) {
									 maxHeight = ah
									 next = v
									 continue = true
								 }
							 }
						 }
					 }

					 peakPoints += tmp

					 val dist = (base - tmp).lengthSafe.max(15)
					 //			val dist = 15
					 outliers = outliers.filter( v => distance(v,tmp) > dist )
				 }

				 for ( p <- peakPoints ) {
					 val h = topological.heightmap(p)
					 val radius = (h - cutoff).toInt * 2

					 for ( dx <- -radius to radius optimized ; dy <- -radius to radius optimized ) {
						 val dist = sqrtf(dx*dx+dy*dy)
						 val basePcnt = (1.0f - dist / radius).max(0.0f) * 0.1f
						 val pcnt = basePcnt * basePcnt

						 if ( pcnt > 0.0f && dist > 0.0f) {
							 val curX = WD.windX(p.x + dx,p.y + dy)
							 val curY = WD.windY(p.y + dx,p.y + dy)

							 WD.windX(p.x + dx,p.y + dy) = curX + (dx / dist) * pcnt
							 WD.windY(p.x + dx,p.y + dy) = curY + (dy / dist) * pcnt
//
//
//							 val angle = atan2f(dy,dx)
//							 val curAngle = WD.directionGrid(p.x + dx,p.y + dy)
//							 var deltaAngle = angle - curAngle
//							 if ( math.abs(deltaAngle) > math.Pi ) {
//								 if ( angle > curAngle ) {
//									 deltaAngle = (angle - math.Pi.toFloat * 2.0f) - curAngle
//								 } else {
//									 deltaAngle = (angle + math.Pi.toFloat * 2.0f) - curAngle
//								 }
//							 }
//
//							 var newAngle = if ( absf(deltaAngle) < 0.1f ) {
//								 curAngle + deltaAngle * pcnt
//							 } else {
//								 curAngle + sign(deltaAngle) * 0.1f * pcnt
//							 }
//
//							 if ( newAngle > math.Pi * 2.0 ) {
//								 newAngle -= math.Pi.toFloat * 2.0f
//							 } else if ( newAngle < 0.0f ) {
//								 newAngle += math.Pi.toFloat * 2.0f
//							 }
//							 WD.directionGrid(p.x + dx,p.y + dy) = newAngle
						 }
					 }
				 }
			 }
		 }

		 Nil
	 }
 }
