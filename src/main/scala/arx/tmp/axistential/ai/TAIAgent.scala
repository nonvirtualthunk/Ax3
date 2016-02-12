package arx.axistential.ai

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/6/13
 * Time: 4:02 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.core.Moddable
import arx.core.representation.InformationLevel.InformationLevel
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.entities.data.TGameEntityAuxData
import arx.tmp.game.networking.TNetworkedGameEntityAuxData
import arx.macros.NetworkedAuxData

import scala.annotation.tailrec
import scala.language.implicitConversions

trait TAIAgent extends GameEntity with TAIEntity {
	protected[ai] var internAIData = createInternAIData()

	def createInternAIData () = {
		val ad = new AIAgentData()
		addAuxData(ad)
		ad
	}

	override protected def onNewAuxDataCreated(gead: TGameEntityAuxData): Unit = {
		super.onNewAuxDataCreated (gead)

		gead match {
			case aid : AIAgentData => internAIData = aid
			case _ =>
		}
	}
}

object TAIAgent {
	implicit def toAIData (agent : TAIAgent) : AIAgentData = agent.internAIData
}


@NetworkedAuxData
class AIAgentData extends TGameEntityAuxData with TNetworkedGameEntityAuxData {
	var aiGroup : TAIGroup = TAIGroup.Sentinel
	@volatile var activeGoal : Option[Goal] = None
	var passiveGoals : List[Goal] = Nil

	var actionRate = Moddable(1.0f)

	def activeLeafGoal : Option[Goal] = activeGoal.map( recurseDown )

	@tailrec
	protected final def recurseDown ( g : Goal ) : Goal = {
		g.subGoals.find( ! _.finished ) match {
			case Some(sb) => recurseDown(sb)
			case _ => g
		}
	}

	override def informationPairs(level: InformationLevel): Map[String, Any] = Map()
}