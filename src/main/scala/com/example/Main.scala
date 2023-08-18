package com.example

import cats.effect._
import cats.effect.kernel.Ref
import cats.syntax.all._
import sttp.client4._
import sttp.client4.httpclient.cats.HttpClientCatsBackend
import io.circe.parser._
import scala.util.Random

/*
These case classes represent a Product and a Cart.
They are immutable data structures, and any modifications will result in a new instance, promoting referential transparency.
Algorithmic data types like case class is a must whenever possible.
 */
case class Product(name: String, price: Double)
case class Cart(items: Map[Product, Int] = Map())

/*
This class encapsulates the HTTP logic for interacting with the product service.
It takes a Backend[IO], allowing us to send requests to retrieve product prices.
 */
class ShoppingService(backend: Backend[IO]) {
  def getProductPrice(url: String): IO[Double] = {
    val request = basicRequest.get(uri"$url")
    for {
      response <- backend.send(request)
      price <- response.body match {
        case Left(error) =>
          IO.raiseError(new Exception(s"Error in response body: $error"))
        case Right(body) =>
          IO.fromEither(
            parse(body).flatMap(_.hcursor.get[Double]("price"))
          )
      }
    } yield price
  }
}

/*
Using .Simple, you only need to implement a single method, run: IO[Unit],
and the IOApp takes care of the boilerplate like executing the IO and handling errors.
 */
object Main extends IOApp.Simple {

  val baseUrls = List(
    "https://raw.githubusercontent.com/mattjanks16/shopping-cart-test-data/main/cheerios.json",
    "https://raw.githubusercontent.com/mattjanks16/shopping-cart-test-data/main/cornflakes.json",
    "https://raw.githubusercontent.com/mattjanks16/shopping-cart-test-data/main/frosties.json",
    "https://raw.githubusercontent.com/mattjanks16/shopping-cart-test-data/main/shreddies.json",
    "https://raw.githubusercontent.com/mattjanks16/shopping-cart-test-data/main/weetabix.json"
  )

  /*
  Updates the shopping cart in a functional way using a Ref.
  A Ref provides a safe way to manage mutable state within the functional paradigm.
  Returns an IO[Unit], which represents the effect of updating the cart,
  without actually performing it until execution.
   */
  def addToCart(
      cart: Ref[IO, Cart],
      product: Product,
      quantity: Int
  ): IO[Unit] = {
    cart.update(cart => {
      val newQuantity = cart.items.getOrElse(product, 0) + quantity
      Cart(cart.items.updated(product, newQuantity))
    })
  }

  /*
  This function calculates the totals for the shopping cart.
  It's a pure function, taking an immutable Cart object as input and returning a tuple with the subtotal, tax, and total.
   */
  def calculateTotals(cart: Cart): (Double, Double, Double) = {
    val subtotal = BigDecimal(cart.items.map { case (product, quantity) =>
      product.price * quantity
    }.sum).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    val tax = BigDecimal(subtotal * 0.125)
      .setScale(2, BigDecimal.RoundingMode.HALF_UP)
      .toDouble
    val total = BigDecimal(subtotal + tax)
      .setScale(2, BigDecimal.RoundingMode.HALF_UP)
      .toDouble
    (subtotal, tax, total)
  }

  /*
  Main program logic, defined as an IO value.
  A Backend[IO] is created, and the ShoppingService is instantiated with it.
  Products are retrieved, added to the cart, and totals are calculated and printed.
  The backend is managed as a resource, ensuring its proper acquisition and release.
   */
  def run: IO[Unit] = {
    HttpClientCatsBackend.resource[IO]().use { backend: Backend[IO] =>
      val shoppingService = new ShoppingService(backend)
      for {
        cartRef <- Ref.of[IO, Cart](Cart())
        _ <- (1 to baseUrls.length).toList.traverse_ { _ =>
          val randomUrl = baseUrls(Random.nextInt(baseUrls.length))
          val randomQuantity = Random.nextInt(10) + 1
          val productName = randomUrl.split("/").last.stripSuffix(".json")
          for {
            price <- shoppingService.getProductPrice(randomUrl)
            _ <- addToCart(cartRef, Product(productName, price), randomQuantity)
          } yield ()
        }
        finalCart <- cartRef.get
        (subtotal, tax, total) = calculateTotals(finalCart)
        _ <- IO(
          println(
            s"Subtotal = $$${subtotal}\nTax = $$${tax}\nTotal = $$${total}"
          )
        )
      } yield ()
    }
  }
}
