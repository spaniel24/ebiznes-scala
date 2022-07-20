package controllers

import models.{FirstItemDTOModel, FirstItemModel}
import play.api.libs.json.{Json, OFormat}
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}

import javax.inject.Inject
import scala.collection.mutable

class SecondController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {

  implicit val itemFormatter: OFormat[FirstItemModel] = Json.format[FirstItemModel]
  implicit val itemDTOFormatter: OFormat[FirstItemDTOModel] = Json.format[FirstItemDTOModel]

  private val itemsList = new mutable.ListBuffer[FirstItemModel]()
  itemsList += FirstItemModel(1, "Pierwszy z drugiej")
  itemsList += FirstItemModel(2, "Drugi z drugiej")
  itemsList += FirstItemModel(3, "Trzeci z drugiej")
  itemsList += FirstItemModel(4, "Czwarty z drugiej")
  itemsList += FirstItemModel(5, "Piąty z drugiej")

  def showAll(): Action[AnyContent] = Action {
    if (itemsList.isEmpty) {
      NoContent
    } else {
      Ok(Json.toJson(itemsList))
    }
  }

  def showById(id: Long): Action[AnyContent] = Action {
    val item = itemsList.find(_.id == id)
    item match {
      case Some(firstItemModel) => Ok(Json.toJson(firstItemModel))
      case None => NotFound
    }
  }

  def update(id: Long): Action[AnyContent] = Action { implicit request =>
    val obj = request.body
    val jsonObj = obj.asJson
    val updatedItem: Option[FirstItemDTOModel] =
      jsonObj.flatMap(Json.fromJson[FirstItemDTOModel](_).asOpt)

    updatedItem match {
      case Some(someItem) =>
        val item = itemsList.find(_.id == id)
        item match {
          case Some(item) =>
            val updItem = item.copy(name=someItem.name)
            itemsList.filterInPlace(_.id != id)
            itemsList += updItem
            Accepted(Json.toJson(updItem))
          case None =>NotFound
        }
      case None => BadRequest
    }
  }

  def delete(id: Long): Action[AnyContent] = Action {
    itemsList.filterInPlace(_.id != id)
    Accepted
  }

  def add(): Action[AnyContent] = Action { implicit request =>
    val content = request.body
    val jsonObject = content.asJson
    val newItem: Option[FirstItemDTOModel] =
      jsonObject.flatMap(
        Json.fromJson[FirstItemDTOModel](_).asOpt
      )

    newItem match {
      case Some(someItem) =>
        val nextId = itemsList.map(_.id).max + 1
        val newItem = FirstItemModel(nextId, someItem.name)
        itemsList += newItem
        Created(Json.toJson(newItem))
      case None =>
        BadRequest
    }
  }
}
