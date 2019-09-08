package arx.samvival.graphics

import arx.engine.entity.Taxon
import arx.engine.lworld.{LEntity, LWorldView}
import arx.graphics.Image
import arx.resource.ResourceManager
import arx.samvival.game.entities.{CharacterInfo, DamageType}
import arx.samvival.game.logic.Leveling

object GameImageManager {

	def imageForCharacter(character : LEntity)(implicit view : LWorldView): Image = {
		val species = character.data[CharacterInfo].species
		val mainClass = Leveling.mainCharacterClass(character)
		imageForCharacterClassAndSpecies(mainClass, species).getOrElse(ResourceManager.image("samvival/characters/human/ruffian.png"))
	}

	def imageForCharacterClassAndSpecies(characterClass : Taxon, species : Taxon) : Option[Image] = {
		val path = s"samvival/characters/${species.name.replace(' ','_')}/${characterClass.name}.png"
		ResourceManager.imageOpt(path) match {
			case None =>
				species.parents.toStream.map(p => imageForCharacterClassAndSpecies(characterClass, p)).find(_.isDefined).flatten match {
					case None =>
						characterClass.parents.toStream.map(p => imageForCharacterClassAndSpecies(p, species)).find(_.isDefined).flatten
					case pi => pi
				}
			case o => o
		}
	}

	def imageForDamageType(damageType : DamageType) = {
		ResourceManager.image(s"samvival/damageTypes/${damageType.name}.png")
	}
}
