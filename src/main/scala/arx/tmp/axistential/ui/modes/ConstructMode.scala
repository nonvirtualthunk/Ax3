package arx.axistential.ui.modes

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/6/13
 * Time: 9:18 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.axistential.ai.Goal
import arx.axistential.game.archetypes.Material
import arx.axistential.game.archetypes.MaterialType
import arx.axistential.game.entities.groups.Colony
import arx.axistential.game.entities.traits.TPhysicalEntity
import arx.axistential.game.logic.ai.goals.BuildVoxelGoal
import arx.axistential.game.logic.ai.goals.MoveGoal
import arx.axistential.game.logic.requirements.MaterialTypeDescriptor
import arx.axistential.game.logic.requirements.SpecificMaterialDescriptor
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.coordinates.VoxelCoord
import arx.tmp.game.logic.datastructures.voxelregions.VoxelRegion
import arx.tmp.game.logic.descriptors.AnyEntityDescriptor
import arx.tmp.game.logic.descriptors.TEntityDescriptor
import arx.tmp.game.logic.entities.core.GameEntity
import arx.tmp.game.logic.world.SpatialRegion

trait TAxistentialGameMode extends TGameMode {
	lazy val colony = world.entitiesOfType[Colony].head

	def hypotheticalGoals : Traversable[Goal]
}

class ConstructMode extends TAxistentialGameMode {

	var anchors : List[VoxelCoord] = Nil
	var subMode : ConstructSubMode = ConstructMode.SubModes.head
	var materialDescriptor : TEntityDescriptor = AnyEntityDescriptor


	def createSubModeRadioWidget ( sm : ConstructSubMode, p : Widget ) = {
		val w = ImageDisplayWidget(image(s"axis/ui/modes/images/Construct${sm.name}Mode.png"),p)
		w.backgroundImage = image("ui/singlePixelBorder_ne.png")
		w
	}
	/*parent will have been set */
	override def onInitialize(): Unit = {
		val w = Widget.fromResource("axis/ui/modes/ConstructWidget.sml",parentWidget)
		val radio = w.getById[RadioSelectionWidget]("submode selection radio")
		radio.configure(ConstructMode.SubModes,createSubModeRadioWidget)


		w.replacements += ("selectedMaterial" -> (() => materialDescriptor.name.capitalize))
		w.replacements += ("constructionSubMode" -> (() => subMode.name))

		val matSelectionButtons = w.getById[Widget]("material selection container")
			.children
			.filter(_.id.startsWith("material selection"))

		matSelectionButtons.foreach ( b => {
			if ( b.dataString.toLowerCase != "other" ) {
				val desc = new MaterialTypeDescriptor(MaterialType(b.dataString))
				if ( materialDescriptor == AnyEntityDescriptor ) { materialDescriptor = desc }
				b.onClick( materialDescriptor = desc )
				b.backgroundColor = () => if ( materialDescriptor == desc ) { Color(0.5f,1.0f) } else { Color.White }
			} else {
				b.backgroundColor = () => materialDescriptor match {
					case mtd : MaterialTypeDescriptor => Color.White
					case _ => Color(0.5f,1.0f)
				}
				b.onClick {
					val matList = new ListWidget(parentWidget)
					matList.configure(Material.allArchetypes,(m:Material,p:Widget) => {
						val tdw = TextButton(m.name.capitalize,p)
						tdw.matchTextDimensions()
						tdw.textAlignment = Center

						tdw.onClick {
							materialDescriptor = SpecificMaterialDescriptor(m)
							matList.close()
						}

						tdw
					})
					matList.x = w.x + w.width
					matList.y = b.y
					matList.width = 15.0f
					matList.height = 20.0f
					matList.makeEphemeral()
				}
			}
		})
	}

	def goalForMouse ( start : ObjectCoord, end : ObjectCoord ) = {
		val intersect = graphicsEngine.intersectWorld(start,end)
		val hypoAnchor = subMode.anchorForMouse(anchors,start,end,intersect)
		if ( anchors.isEmpty && hypoAnchor.isEmpty ) {
			None
		} else {
			Some( goalForAnchors(anchors ::: hypoAnchor.toList) )
		}
	}
	def goalForAnchors ( hypoAnchors : List[VoxelCoord] ) = {
		val sel = subMode.regionForAnchors(hypoAnchors)
		BuildVoxelGoal( sel , new MaterialTypeDescriptor(Material.Stone))
	}



	onEvent{
		case WorldMousePressEvent(button,start,end,modifiers) => {
			if (modifiers.ctrl) {
				graphicsEngine.intersectWorld(start,end) match {
					case Some(wir) => colony.addGoal(MoveGoal(VoxelRegion(wir.voxel + dirvec(wir.side))))
					case None =>
				}
			} else {
				subMode.anchorForMouse(anchors,start,end,graphicsEngine.intersectWorld(start,end)) match {
					case Some(newAnchor) => {
						anchors :+= newAnchor
						if ( subMode.isComplete(anchors) ) {
							colony.addGoal( goalForAnchors(anchors) )
							anchors = Nil
						}
					}
					case None =>
				}
			}
		}
		case KeyPressEvent(key,modifiers) => {
			if ( key == Keyboard.KEY_EQUALS ) { gameEngine.timescale *= 2.0f }
			if ( key == Keyboard.KEY_MINUS ) { gameEngine.timescale /= 2.0f }
			subMode.fireEvent(KeyPressEvent(key,modifiers))
			false
		}
	}


	def hypotheticalGoals = goalForMouse(currentWorldMouseLine._1,currentWorldMouseLine._2).toList
}



abstract class ConstructSubMode extends AnchorSelection with TEventUser {
	def name : String
	def settings : List[Setting[_]]

}
class WallSubMode extends ConstructSubMode{
	def name = "Wall"

	val height = new IntRangeSetting("height",2.meters.inVoxels.toInt,1,20,1)
	val minimal = new BooleanSetting("minimal",default = false)

	def settings: List[Setting[_]] = height :: minimal :: Nil

	def anchorForMouse(existingAnchors: List[VoxelCoord], start: ObjectCoord, end: ObjectCoord, intersect: Option[WorldIntersectionResult]): Option[VoxelCoord] = {
		intersect match {
			case Some(WorldIntersectionResult(vox,_,side)) => Some(vox + dirvec(side))
			case _ => None
		}
	}

	def regionForAnchors(anchors: List[VoxelCoord]) = {
		val (min,max) = anchors.head.minMax(anchors.last)
		max.z = math.max( min.z + height.value , max.z )

		VoxelRegion(VoxelCoord(min),VoxelCoord(max))
	}

	onEvent {
		case KeyPressEvent(key,_) => {
			if (key >= Keyboard.KEY_1 && key <= Keyboard.KEY_9) {
				height.value = 8 - (Keyboard.KEY_9 - key)
			}
		}
	}
}



object ConstructMode {
	var SubModes = List( new WallSubMode )
}

trait AnchorSelection {
	def numAnchors = 2
	def isComplete ( anchors : List[VoxelCoord] ) : Boolean = numAnchors == anchors.size
	def anchorForMouse ( existingAnchors : List[VoxelCoord], start : ObjectCoord , end : ObjectCoord , intersect : Option[WorldIntersectionResult] ) : Option[VoxelCoord]
	def regionForAnchors ( anchors : List[VoxelCoord] ) : VoxelRegion
}


class WorldSelection (gameMode : TGameMode,intersectEntities:Boolean) {
	def world = gameMode.gameEngine.world
	def graphicsEngine = gameMode.graphicsEngine

	var origin : Option[Either[VoxelCoord,TPhysicalEntity]] = None
	var mouseOver : Option[Either[VoxelCoord,TPhysicalEntity]] = None
	
	def update ( worldMouseLine : (ObjectCoord,ObjectCoord) ) {
		
	}

	def activeSelection = {
		selectionFromTwo(origin,mouseOver)
	}

	protected def selectionFromSingle ( point : Option[Either[VoxelCoord,TPhysicalEntity]] ) : Option[Either[SpatialRegion,TPhysicalEntity]] = {
		point match {
			case Some(p) => {
				p match {
					case Cardinals.Left(v) => Some(Cardinals.Left(SpatialRegion.fromCorners(v,v)))
					case Cardinals.Right(pe) => Some(Cardinals.Right(pe))
				}
			}
			case None => None
		}
	}
	protected def selectionFromTwo ( a : Option[Either[VoxelCoord,TPhysicalEntity]] , b : Option[Either[VoxelCoord,TPhysicalEntity]] ) : Option[Either[SpatialRegion,TPhysicalEntity]] = {
		if ( a.nonEmpty && b.nonEmpty ) {
			val (sa,sb) = (a.get,b.get)
			if ( sa.isRight && sb.isRight ) {
				val (ape,bpe) = (sa.right.get,sb.right.get)
				if ( ape == bpe ) { //if we selected the same entity for both points, then just it
					Some(Cardinals.Right(ape))
				} else { //otherwise, it's a volume selection still
					Some(Cardinals.Left(SpatialRegion.fromTwoPoints( ape.adjustedFootVoxelPos , bpe.adjustedFootVoxelPos )))
				}
			} else {
				Some(Cardinals.Left(SpatialRegion.fromTwoPoints( extractPoint(sa), extractPoint(sb) )))
			}
		} else if ( a.isEmpty ) {
			selectionFromSingle(b)
		} else {
			selectionFromSingle(a)
		}
	}
	protected def extractPoint ( e : Either[VoxelCoord,TPhysicalEntity] ) = e match {
		case Cardinals.Left(v) => v
		case Cardinals.Right(pe) => pe.adjustedFootVoxelPos
	}

	gameMode.onEvent {
		case WorldMousePressEvent(button,start,end,modifiers) => {
			val intersections = 
				if ( intersectEntities ) { graphicsEngine.intersectAll(start,end,None,None) }
				else { graphicsEngine.intersectWorld(start,end).toList }
			
			if ( intersections.nonEmpty ) {
				intersections.head match {
					case WorldIntersectionResult(voxel,point,side) => {
						origin = origin match {
							case Some(existingOrigin) => {
								val originV = existingOrigin match { case Cardinals.Left(v) => v ; case Cardinals.Right(ent) => ent.adjustedFootVoxelPos }

								gameMode.handleEvent( VolumeSelectedEvent(SpatialRegion.fromTwoPoints(originV,voxel+dirvec(side))) )
								None
							}
							case None => Some( Cardinals.Left(VoxelCoord(voxel+dirvec(side))) )
						}
					}
					case EntityIntersectionResult(entity,point) => {
						entity match {
							case pe : TPhysicalEntity => origin = origin match {
								case None => Some( Cardinals.Right(pe) )
								case Some(existingOrigin) => {
									existingOrigin match {
										case Cardinals.Right( otherEnt ) => {
											if ( otherEnt == pe ) { gameMode.handleEvent( EntitySelectedEvent(pe) ) }
											else {
												gameMode.handleEvent( VolumeSelectedEvent( SpatialRegion.fromTwoPoints( pe.adjustedFootVoxelPos , otherEnt.adjustedFootVoxelPos ) ) )
											}
										}
										case Cardinals.Left( v ) => {
											gameMode.handleEvent( VolumeSelectedEvent( SpatialRegion.fromTwoPoints( pe.adjustedFootVoxelPos , v ) ) )
										}
									}
									None
								}
							}
						}
					}
					case _ => Noto.warn("Unknown intersection type encountered by world selection")
				}
				true
			} else {
				false
			}
		}
		case KeyPressEvent(key,modifiers) if key == Keyboard.KEY_ESCAPE && origin.isEmpty => {
			origin = None
			true
		}
		case _ => false
	}
}
case class VolumeSelectedEvent ( region : SpatialRegion ) extends Event
case class EntitySelectedEvent ( entity : GameEntity ) extends Event