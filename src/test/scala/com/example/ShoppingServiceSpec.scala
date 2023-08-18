package com.example

import cats.effect._
import munit.CatsEffectSuite
import sttp.client4._
import sttp.client4.testing._
import sttp.client4.httpclient.cats.HttpClientCatsBackend
import sttp.model.StatusCode

class ShoppingServiceSuite extends CatsEffectSuite {
  val stubBackend = HttpClientCatsBackend.stub[IO]

  test("ShoppingService should return the correct price for a product") {
    val jsonResponse = """{"price": 10.0}"""
    val testBackend = stubBackend
      .whenRequestMatches(_.uri.toString == "test-url")
      .thenRespond(Response(Right(jsonResponse), StatusCode.Ok))

    val service = new ShoppingService(testBackend)

    service.getProductPrice("test-url").map { price =>
      assertEquals(price, 10.0)
    }
  }

  test("ShoppingService should handle backend errors gracefully") {
    val testBackend = stubBackend
      .whenRequestMatches(_.uri.toString == "test-url")
      .thenRespondServerError()
    val service = new ShoppingService(testBackend)

    service.getProductPrice("test-url").intercept[Exception]
  }

  test("addToCart should correctly add products to the cart") {
    Ref.of[IO, Cart](Cart()).flatMap { cart =>
      val product = Product("test-product", 10.0)
      Main.addToCart(cart, product, 2) *> cart.get.map { finalCart =>
        assertEquals(finalCart.items, Map(product -> 2))
      }
    }
  }

  test("calculateTotals should correctly calculate the subtotal, tax, and total") {
    val cart = Cart(Map(Product("test-product", 10.0) -> 2))
    val (subtotal, tax, total) = Main.calculateTotals(cart)
    assertEquals(subtotal, 20.0)
    assertEquals(tax, 2.5)
    assertEquals(total, 22.5)
  }

}
