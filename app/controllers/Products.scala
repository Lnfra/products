
package controllers

import play.api.mvc.{Action, Controller}
import models.Product
import play.api.data._
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.mvc.Flash

object Products extends Controller {

  //The form's fields and their constraints
  private val productForm: Form[Product] = Form(
    mapping(
      "ean" -> longNumber.verifying(
        "validation.ean.duplicate", Product.findByEan(_).isEmpty),
       "name" -> nonEmptyText,
       "description" -> nonEmptyText
    )(Product.apply)(Product.unapply) //Functions to map between form and model
  )

  //Controller action
  def list = Action { implicit request =>
    //Get a product list from model
    val products = Product.findAll
    
    //Render view template
    Ok( views.html.products.list(products))
  }

  def show(ean: Long) = Action { implicit request =>

    Product.findByEan(ean).map { product => 
      Ok( views.html.products.details(product))
    }.getOrElse(NotFound)
  }

  //Validate and save data from new product form 
  def save = Action { implicit request =>
   
    //Bind the request parameters with the form fields, matches by name
    //Validation happens during the binding
    val newProductForm = productForm.bindFromRequest()

    newProductForm.fold(
     //If validation fails redirect back to Add page
     //If validation pass save new product and redirect to details page
      hasErrors = { form =>
        Redirect(routes.Products.newProduct()).
        flashing(Flash(form.data) + ("error" -> Messages("validation.errors")))
      },
      success = { newProduct =>
        Product.add(newProduct)
        val message = Messages("products.new.success", newProduct.name)
        Redirect(routes.Products.show(newProduct.ean)).
        flashing("success" -> message)
      }
    )
  }

  def newProduct = Action { implicit request =>
    val form = {
      if (flash.get("error").isDefined)
         productForm.bind(flash.data)
      else
        productForm
    }

    Ok(views.html.products.editProduct(form))
  }
}
