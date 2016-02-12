package arx.axistential.game.logic.ai.goals

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/17/13
 * Time: 10:59 AM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.ai.AI.Reason._
import arx.axistential.ai._
import arx.axistential.ai.traits.TSingularGoal
import arx.axistential.game.data.world.TerrainData
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.entities.CreaturePropertiesData
import arx.axistential.game.entities.CreatureStateData
import arx.axistential.game.logic.ai.AxisSearcher
import arx.axistential.game.logic.general.MovementLogic
import arx.axistential.modules.hunger.ActivityLevel
import arx.core.gen.SimplexNoise
import arx.core.units.NoSpeed
import arx.core.units.UnitOfTime
import arx.core.units.Velocity
import arx.core.vec.Vec3f
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.Talea
import arx.tmp.game.logic.datastructures.voxelregions.VoxelRegion
import arx.tmp.game.logic.functions.TimeLimitedFunction

import scala.language.postfixOps
import scalaxy.loops._


case class MoveGoal (
  acceptableDestinations : VoxelRegion,
  providedPath : Option[List[VoxelCoord]] = None,
  gait : MoveGait = MoveGait.Walk
) extends PhysicalGoal with TSingularGoal {
	def this(v : VoxelCoord) { this(VoxelRegion(v)) }


	override def activityLevel: ActivityLevel = gait match {
		case MoveGait.Saunter => ActivityLevel.Minimal
		case MoveGait.SlowWalk => ActivityLevel.VeryLight
		case MoveGait.Walk => ActivityLevel.Light
		case MoveGait.Jog => ActivityLevel.Moderate
		case MoveGait.Run => ActivityLevel.Heavy
		case MoveGait.Sprint => ActivityLevel.Extreme
		// Sneak is a little different, it's the speed of a SlowWalk, but is presumed to require
		// much more effort as it requires significant care and control.
		case MoveGait.Sneak => ActivityLevel.Moderate
	}


	override def toString: String = s"MoveGoal(destinations: ${acceptableDestinations})"

	def createPrerequisiteGoals(agent: TAIAgent) = Nil
	def fitness(agent: TAIAgent): Int = 0

	var internProvidedPath = providedPath
	var path : List[VoxelCoord] = Nil
	var underPath : List[Byte] = Nil
	var beaconPoints = List[ObjectCoord]()
	var beaconIndex = 1
	var lastPosition : ObjectCoord = ObjectCoord.Sentinel
	var ticksWithoutProgress = 0

	var plannedFrom : ObjectCoord = ObjectCoord.Sentinel
	var anyStepsTaken = false

	override def onReset () {
		// we don't want to use a provided path more than once, if we've reset then that provided path
		// is no longer reliable
		internProvidedPath = None
		path = Nil
		beaconPoints = Nil
		underPath = Nil
		beaconIndex = 1
		plannedFrom = ObjectCoord.Sentinel
		anyStepsTaken = false
	}

	override def plan(agent : TAIAgent): AIResult = {
		agent match {
			case pe : TPhysicalEntity => {
				val terrain = agent.world.aux[TerrainData]

				plannedFrom = pe.adjustedFootPos
				val obstructionFunc = MovementLogic.obstructionFunction(agent.world)

				val startObstructed = obstructionFunc(agent.adjustedFootVoxelPos)

				internProvidedPath = internProvidedPath match {
					case Some(p) => p.exists(obstructionFunc) match {
						case true => None
						case false => Some(p)
					}
					case None => None
				}

				// provided path if present, attempted search if absent
				val foundPath = internProvidedPath.orPossibly(
					AxisSearcher.pathTo(
						AxistentialPathQuery(
							pe,
							acceptableDestinations,
							200,
							MovementLogic.moveCostFunction(agent),
							(v) => true,
							obstructionFunc,
							MovementLogic.isSupportedFunction(agent.world)
						)
					)
				)

				foundPath match {
					case None => {
						if (startObstructed) {
							Retry( AI.Reason.Stuck )
						} else {
							// If we did not find any acceptable path, abort, taking note of what the modification
							// counts were for all of the relevant taleae. Then we can retry if those change.
							val allTaleaRevisions = (pe.adjustedFootVoxelPos :: acceptableDestinations.toList)
								.map(v => VoxelCoord((v >> Talea.dimensionPo2) << Talea.dimensionPo2))
								.distinct
								.map(v => v -> terrain.materialGrid.taleaFor(v.x,v.y,v.z).modifiedCount)
								.toMap
							Abort( NoPath(pe.adjustedFootVoxelPos,acceptableDestinations,allTaleaRevisions) )
						}
					}
					case Some(p) => {
						// Store our newfound path, as well as a record of what voxels are under each step
						path = p
						underPath = p.map( v => terrain.materialByteAt(v.minusZ(1)) )
						// Remove unnecessary intermediary steps when moving upward? Why are we doing that
						val adjustedPath = removeUnnecessaryIntermediaryPathPoints(p)

						recreateBeaconPoints(pe, adjustedPath, obstructionFunc)

						// Check to ensure that we haven't somehow created a path with points that cannot actually
						// be moved through. In theory this should never happen
						if ( beaconPoints.exists( bp => obstructionFunc(bp.toVoxelCoord) ) ) {
							val badBeaconPoints = beaconPoints.filter( bp => obstructionFunc(bp.toVoxelCoord) )

							// This has been hit on a mountain-side movement situation
							Retry(UnexpectedState(s"Bad beacon points $badBeaconPoints"))
						} else {
							Success
						}
					}
				}
			}
			case _ => Fail( InvalidAgent(agent) )
		}
	}


	def act(agent: TAIAgent, dt : UnitOfTime): AIResult = {
		agent match {
			case pe : TPhysicalEntity => {
				val obstrFunc = MovementLogic.obstructionFunction(agent.world)
				val suppFunc = MovementLogic.isSupportedFunction(agent.world)

				val newSpeed = updateMovementSpeed(pe,dt)

				// approximate distance that will be moved this tick, used to determine if we need to avance beacons
				val movementThisTick = dt.inSeconds * newSpeed

				// TODO : make our testing for finished closeness actually tie in properly with our beacon index incrementation

				// if one of our acceptable destinations, which is not obstructed and is supported, is sufficiently close to our current position
				val curVoxPos = pe.adjustedFootVoxelPos
				if ( acceptableDestinations.contains(curVoxPos) &&
						! obstrFunc(curVoxPos) &&
						suppFunc(curVoxPos) &&
						curVoxPos.toObjectCoordFoot.distanceTo(pe.adjustedFootPos) < (movementThisTick * 2.1f).voxels )
				{
					resetCurrentSpeed(agent)
					AIResult.Success
				} else {
					val terrain = agent.world.aux[TerrainData]

					// if there are no more beacon points, but we haven't reached a valid destination (the above condition),
					// then something is wrong with our plan
					if ( beaconIndex >= beaconPoints.size ) {
						Abort(OutdatedPlan)
					// if we're more than a a voxel and change away from our intended beacon we've gone off course somehow
					} else if ( (pe.adjustedFootPos - beaconPoints(beaconIndex)).abs.max > 1.25 * gait.speedMultiplier.max(1.0f) ) {
						if (anyStepsTaken) { Retry( UnexpectedState("off course") ) }
						else { Retry(OutdatedPlan) }
					// if the block underneath any of our beacons from current to then end is not what we expect, we'll need to re-plan
					} else if ( ! (beaconIndex-1 until path.size).forall( i => underPath(i) == terrain.materialByteAt( path(i).minusZ(1) ) ) ) {
						Retry( UnexpectedState("ground changed") )
					// and if any of the blocks in our path are now solid, we're going to need to re-plan as well
					// TODO : Have this check obstructions for z <- 0 until height, at least for nearby beacon points
					} else if ( (beaconIndex-1 until path.size).exists( i => terrain.isSolid( path(i) ) ) ) {
						Retry( UnexpectedState("obstructions changed") )
					// otherwise, we're still on track, the path hasn't changed, keep moving
					} else {
						val makingProgress = checkForMovementProgress(pe)
						if (! makingProgress) { return Retry(Stuck) }

						setNextMovementVelocity(pe, movementThisTick, newSpeed)

						Continue

					}
				}
			}
			case _ => Fail( InvalidAgent(agent) )
		}
	}

	/** Sets the agent's motive vector such that it will be moving towards the next beacon point as best
	  * as possible. Updates the beacon index whenever a beacon point is sufficiently close
	  */
	def setNextMovementVelocity(pe: TAIAgent with TPhysicalEntity, movementThisTick : Float, newSpeed : Float) = {
		var continue = true
		while (continue && beaconIndex < beaconPoints.size) {
			continue = false
			val prev = beaconPoints(beaconIndex-1)
			val next = beaconPoints(beaconIndex)
			val delta = next - pe.adjustedFootPos
			val nDelta = delta.normalizeSafe
			val deltaLength = delta.lengthSafe

			//									Noto.debug(s"[move] prev: $prev, next: $next, delta: $delta, nDelta: $nDelta")

			// if we're hopping, moving up and horizontally in some direction
			val movingXY = ! next.x.aeq(prev.x,0.1f) || ! next.y.aeq(prev.y,0.1f)
			if ( next.z > prev.z && movingXY ) {
				// and we'll reach our target horizontal this tick, and we're high enough, advance to the next beacon
				if ( delta.xy.lengthSafe < movementThisTick * 2.0f && delta.z < 0.0f ) {
					beaconIndex += 1
					continue = true
				} else {
					//-2.515x^2 + 1.565x - 0.05    position, as function of xy distance

					val totalXYDist = clamp((next - prev).xy.lengthSafe,0.0f,1.0f)

					val xyDist = math.min(1.0f,delta.xy.lengthSafe / totalXYDist)
					// compute the ideal z value, as given by a curve based on xy-distance from the beacon
					val desiredZ = next.z + -2.515f * xyDist * xyDist + 1.565f * xyDist - 0.05f
					val normalizedXYDelta = delta.xy.normalizeSafe
					pe.motiveVector = Velocity(
						(normalizedXYDelta.x * newSpeed * 0.5f).v_s,
						(normalizedXYDelta.y * newSpeed * 0.5f).v_s,
						((desiredZ - pe.adjustedFootPos.z) * 8.0f * gait.speedMultiplier.max(1.0f)).v_s
					)
					pe.ignoringGravity = true
				}
			} else {
				if ( deltaLength < movementThisTick * 2.0f ) {
					beaconIndex += 1
					continue = true
				} else {
					val adjustedDelta = nDelta * newSpeed * 0.75f
					pe.motiveVector = Velocity(
						adjustedDelta.x.v_s,
						adjustedDelta.y.v_s,
						if ( adjustedDelta.z >= 0.0f ) { adjustedDelta.z.v_s } else { NoSpeed }
					)
					pe.ignoringGravity = adjustedDelta.z >= 0.0f
				}
			}
		}

//		pe.world.aux[DebugData].graphingData ::= pe.world.time.inSeconds -> pe.motiveVector.inVoxelsPerSecond.xy.lengthSafe
//		val nd = pe.motiveVector.z.inVoxelsPerSecond
//		val nd = pe.motiveVector.inVoxelsPerSecond.xy.lengthSafe

//		val nd = newSpeed
//		pe.world.aux[DebugData].graphingData ::= pe.world.time.inSeconds -> nd
	}

	/** Accelerates/decelerates the entity by it's configured rate to bring it towards its target speed
	  * as determined by it's gait and current speed. Returns the new speed after updating.
	  */
	def updateMovementSpeed(pe : TPhysicalEntity, dt : UnitOfTime) = {
		if (! pe.hasAuxData[CreaturePropertiesData] || ! pe.hasAuxData[CreatureStateData]) {
			Noto.warn(s"MoveGoal does not currently support non-creature entities, creature data will be created if no present.")
			Noto.warn(s"\tProblematic entity is $pe with archetype ${pe.archetype}")
		}
		val CPD = pe.aux[CreaturePropertiesData]
		val CSD = pe.aux[CreatureStateData]
		val baseSpeed = CPD.baseMovementSpeed.inVoxelsPerSecond
		val targetSpeed = gait.speedMultiplier * baseSpeed
		val currentSpeed = CSD.currentMovementSpeed.inVoxelsPerSecond
		val newSpeed = if (! (currentSpeed =~= targetSpeed)) {
			if (currentSpeed < baseSpeed) {
				math.min(baseSpeed,targetSpeed)
			} else {
				val delta = targetSpeed - currentSpeed
				val accel = (CPD.movementAcceleration * dt).inVoxelsPerSecond
				if (delta.abs > accel) {
					currentSpeed + sign(delta).toFloat * accel
				} else {
					targetSpeed
				}
			}
		} else { targetSpeed }

		CSD.currentMovementSpeed = new TimeLimitedFunction(newSpeed.v_s,0.v_s,pe.world,1.second)
		CSD.currentMovementGait = new TimeLimitedFunction(gait,MoveGait.Stationary,pe.world,1.second)

		newSpeed
	}

	/** Checks to determine if the agent is making any progress, and if it is not, indicating that fact
	  * so that the movement goal can retry/replan as necessary. */
	def checkForMovementProgress(pe: TAIAgent with TPhysicalEntity) = {
		anyStepsTaken = true
		if ( pe.position.scalarDistanceTo(lastPosition) < 0.000001f ) {
			ticksWithoutProgress += 1
		} else {
			ticksWithoutProgress = 0
		}

		lastPosition = pe.position

		if ( ticksWithoutProgress > 60 ) {
			val cPos = pe.adjustedFootVoxelPos.toObjectCoordFoot
			pe.motiveVector = Velocity(
				(cPos.x - pe.adjustedFootPos.x).v_s,
				(cPos.y - pe.adjustedFootPos.y).v_s,
				(cPos.x - pe.adjustedFootPos.z).v_s
			)
			false
		} else {
			true
		}
	}

	/** Creates beacon points from a path. This allows us to shape the continuous path taken more
	  * closely than simply by the voxels it will pass through
	  */
	def recreateBeaconPoints(pe : TPhysicalEntity with TAIAgent, adjustedPath: List[VoxelCoord], obstructionFunc : (VoxelCoord) => Boolean) = {
		// Reset the beacon points, then recreate them from the adjusted path
		beaconPoints = Nil
		for (i <- 0 until adjustedPath.size optimized) {
			// Grab the baseline voxel coord for this path
			val cur = adjustedPath(i)
			// Compute a raw exact location by converting to object coord foot, plus a small bias
			val rawF = cur.toObjectCoordFoot
			// Compute an adjusted placement to use by doing a small random offset in x and y, this helps
			// to make movement look slightly more natural when multiple actors are involved and moving similarly
			// We only do this after the first few steps to prevent reverse movement and flickering
			val curF = if (i > 1) { rawF + MoveGoalCompanion.tweakFor(rawF,pe) } else { rawF }
			// If we're at the first step in the path always add the rawF exactly
			// TODO: Or, actually, should we add our character's current position?
			// The following loop adds points that are between each step and the next, so we want it to execute
			// for the first step in addition to adding its point directly, which is why this if
			// is separate
			if ( i == 0 ) { beaconPoints ::= rawF }


			// If we're at the last step, we also want to use the exact rawF point, no randomized offsets
			if ( i == adjustedPath.size - 1 ) { beaconPoints ::= rawF }
			else {
				// Otherwise, look at our next step in the path, adjust to object coord (with small bias)
				val next = adjustedPath(i+1)
				val nextF = next.toObjectCoordFoot

				// If we're moving level, just take the midpoint of the two and add it to the path
				if ( next.z == cur.z ) {
					beaconPoints ::= (curF + nextF) * 0.5f
					// Otherwise, if we're moving downward take the midpoint in xy, but the current z
				} else if ( next.z < cur.z ) {
					beaconPoints ::= ObjectCoord((curF.x+nextF.x)*0.5f,(curF.y+nextF.y)*0.5f,curF.z)
					// Else if we're moving upward take the midpoint in xy, but the next z
				} else if ( next.z > cur.z ) {
					beaconPoints ::= ObjectCoord((curF.x+nextF.x)*0.5f,(curF.y+nextF.y)*0.5f,nextF.z)
				}
			}
		}
		// We've been prepending the points, so reverse to put in the correct order
		beaconPoints = beaconPoints.reverse

		// If we're already at the right voxel (and so have no path to follow), but off-middle we'll
		// need to get centered
		if (beaconPoints.isEmpty) {
			val p = pe.adjustedFootPos
			if (obstructionFunc(p.toVoxelCoord)) {
				val p2 = p.plusZ(0.05f)
				if (!obstructionFunc(p2.toVoxelCoord)) {
					beaconPoints = List(p2,p2.toVoxelCoord.toObjectCoordFoot)
				} else { Nil }
			} else {
				beaconPoints = List(p,p.toVoxelCoord.toObjectCoordFoot)
			}
		}
	}



	def removeUnnecessaryIntermediaryPathPoints(p: List[VoxelCoord]) = {
		p.zipWithIndex.filter {
			case (v, index) => {
				// Always keep start and end
				if (index == 0 || index == p.size - 1) {true}
				else {
					val prev = p (index - 1)
					val next = p (index + 1)
					// Keep if current step is higher/equal last step, or next step is not equal to current
					v.z - prev.z <= 0 || (next.z != v.z)
				}
			}
		}.unzip._1
	}



	def progressRequired(agent: TAIAgent): Float = 0

	override def onAbort(agent: TAIAgent, reason: AIReason): Unit = resetCurrentSpeed(agent)
	override def onFail(agent: TAIAgent, reason: AIReason): Unit = resetCurrentSpeed(agent)

	protected def resetCurrentSpeed(agent : TAIAgent): Unit = {
//		val CSD = agent.aux[CreatureStateData]
//		CSD.currentMovementSpeed = 0.m_s
//		CSD.currentMovementGait = MoveGait.Stationary
	}
}

protected[goals] object MoveGoalCompanion {
	/** Calculates a slight position offset tweak based on the coordinate and agent. Should always be deterministic,
	  * intended to reduce the overlap of agents moving along similar paths */
	def tweakFor(coord: ObjectCoord, agent: TAIAgent) = {
		val xT = tweakSimplex(coord.x,coord.y,coord.z + agent.uid)
		val yT = tweakSimplex(coord.z,coord.y,coord.x + agent.uid)
		Vec3f(xT,yT,0.0f)
	}

	import arx.core.gen.ArxGenerators._
	val tweakSimplex = Simplex(new SimplexNoise(System.currentTimeMillis())) >> Mult(0.05f)
}
