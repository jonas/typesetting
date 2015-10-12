/*
 * Copyright 2015 Jonas Fonseca
 * Copyright 2010-2015 Jon Pretty, Propensive Ltd.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.github.jonas.typesetting

import com.typesafe.config.{ ConfigFactory, Config }

import scala.util.Try
import scala.concurrent.duration.Duration
import scala.collection.JavaConversions._
import scala.annotation.implicitNotFound

trait MethodConstraint

trait `Settings.parse` extends MethodConstraint

case class MissingSetting(name: String) extends
    Exception(s"the Settingeter --$name was missing")

@implicitNotFound("Can not combine elements of type ${A} and ${B}")
trait Construct[-A <: Settings, -B <: Settings] { construct =>
  type And <: Settings
  type Or <: Settings

  def and(a: A, b: B): ProductSettings[And]
  def or(a: A, b: B): CoproductSettings[Or]

  def swap: Construct[B, A] { type And = construct.And; type Or = construct.Or } =
    new Construct[B, A] {
      type And = construct.And
      type Or = construct.Or

      def and(a: B, b: A): ProductSettings[And] = construct.and(b, a)
      def or(a: B, b: A): CoproductSettings[Or] = construct.or(b, a)
    }
}

trait Construct_1 {

  implicit def general[A <: Settings, B <: Settings]: Construct[A, B] { type And =
      A with B; type Or = A with B } = {

    new Construct[A, B] {
      type And = A with B
      type Or = A with B

      def and(a: A, b: B) = ProductSettings[A with B](Set(a, b))
      def or(a: A, b: B) = CoproductSettings[A with B](Vector(a, b))
    }
  }
}

object Construct extends Construct_1 {

  implicit def leftProduct[A <: Settings, B <: SimpleSetting[_]]: Construct[ProductSettings[A], B] {
      type And = A with B; type Or = ProductSettings[A] with B } = {

    new Construct[ProductSettings[A], B] {
      type And = A with B
      type Or = ProductSettings[A] with B

      def and(a: ProductSettings[A], b: B) = ProductSettings[A with B](a.elements + b)

      def or(a: ProductSettings[A], b: B) =
        CoproductSettings[ProductSettings[A] with B](Vector(a, b))
    }
  }

  implicit def rightProduct[A <: SimpleSetting[_], B <: Settings]: Construct[A, ProductSettings[B]] {
      type And = B with A; type Or = ProductSettings[B] with A } = leftProduct[B, A].swap

  implicit def leftCoproduct[A <: Settings, B <: SimpleSetting[_]]: Construct[CoproductSettings[A],
      B] { type And = CoproductSettings[A] with B; type Or = A with B } = {

    new Construct[CoproductSettings[A], B] {
      type And = CoproductSettings[A] with B
      type Or = A with B

      def and(a: CoproductSettings[A], b: B) =
        ProductSettings[CoproductSettings[A] with B](Set(a, b))

      def or(a: CoproductSettings[A], b: B) = CoproductSettings[A with B](a.elements :+ b)
    }
  }

  implicit def rightCoproduct[A <: SimpleSetting[_], B <: Settings]: Construct[A,
      CoproductSettings[B]] { type And = CoproductSettings[B] with A; type Or = B with A } =
      leftCoproduct[B, A].swap
}

trait Settings { Settings =>
  type Result

  def parse(args: Config): Result

  def &[B <: Settings](b: B)(implicit con: Construct[Settings.type, b.type]):
      ProductSettings[con.And] = {

    con.and(this, b)
  }

  def |[B <: Settings](b: B)(implicit con: Construct[Settings.type, b.type]):
      CoproductSettings[con.Or] = {

    con.or(this, b)
  }

  def unary_~ : OptionSettings[this.type] = OptionSettings(this)

  def by[R](fn: Result => R): Setting.Handler[this.type, R] =
    new Setting.Handler[this.type, R](this) {
      type From = Result
      def handle(v: From): R = fn(v)
    }
}

case class OptionSettings[Ps <: Settings](Settings: Ps) extends Settings {
  type Result = Option[Settings.Result]

  def parse(args: Config): Result =
    Try(Settings.parse(args)).toOption

  override def toString = s"[$Settings]"
}

case class ProductSettings[Ps <: Settings](elements: Set[Settings]) extends Settings {
  type ProductTypes = Ps
  type Result = Product[ProductTypes]

  def parse(args: Config): Result = {
    val map = elements.map(key => key -> key.parse(args))
    new Product[Ps](map.toMap)
  }

  override def toString = elements.mkString("( ", " & ", " )")
}

case class CoproductSettings[Ps <: Settings](elements: Vector[Settings]) extends Settings {
  type CoproductTypes = Ps
  type Result = Coproduct[CoproductTypes]

  def parse(args: Config): Result = {
    val elems = elements.to[List].flatMap { k =>
      Option(k.parse(args)).map(k -> _)
    }

    elems match {
      case (key, res) :: _ => Coproduct[CoproductTypes](key -> res)
      case Nil => throw new MissingSetting(toString)
    }
  }

  override def toString = elements.mkString("( ", " | ", " )")
}

case class SimpleSetting[T: Setting.Extractor](val path: String) extends Settings {
    simpleSetting =>

  type Result = T

  def checkValue: Option[T] = None

  def filter(fn: T => Boolean): SimpleSetting[T] = new SimpleSetting[T](path) {
    override def checkValue = simpleSetting.checkValue
  }

  protected val extractor: Setting.Extractor[T] = implicitly[Setting.Extractor[T]]

  def parse(args: Config): Result =
    extractor(args, path)

  // Also consider `extractor` in `hashCode` and `equals`
  override def hashCode = path.hashCode ^ extractor.hashCode

  override def equals(that: Any) = that match {
    case that: SimpleSetting[_] =>
      path == that.path && that.extractor == extractor
    case _ =>
      false
  }

  override def toString = path

  def of(v: T): SimpleSetting[T] = new SimpleSetting[T](path) {
    override def checkValue = Some(v)
  }
}

object Setting {

  case class MemorySize(bytes: Long)

  object Extractor {
    def apply[T](f: Config => String => T): Extractor[T] =
      (config: Config, path: String) => f(config)(path)

    implicit val booleanExtractor = Extractor[Boolean](_.getBoolean)
    implicit val intExtractor = Extractor[Int](_.getInt)
    implicit val longExtractor = Extractor[Long](_.getLong)
    implicit val numberExtractor = Extractor[Number](_.getNumber)
    implicit val doubleExtractor = Extractor[Double](_.getDouble)
    implicit val javaDoubleSeqExtractor = Extractor[Seq[java.lang.Double]](_.getDoubleList)
    implicit val doubleSeqExtractor = javaDoubleSeqExtractor.map(_.map(_.toDouble))
    implicit val bytesExtractor = Extractor[Long](_.getBytes).map(MemorySize.apply)
    implicit val stringExtractor = Extractor[String](_.getString)
    implicit val stringSeqExtractor = Extractor[Seq[String]](_.getStringList)
    implicit val durationExtractor = stringExtractor.map(Duration.apply)

    implicit def tryExtractor[T](implicit f: Extractor[T]) =
      Extractor[Try[T]](config => path => Try(f(config, path)))

    implicit def optionExtractor[T: Extractor]: Extractor[Option[T]] =
      implicitly[Extractor[Try[T]]].map(_.toOption)
  }

  trait Extractor[T] extends Function2[Config, String, T]

  implicit class ExtractorOps[T](val extractor: Extractor[T]) extends AnyVal {
    def map[S](f: T => S) = Extractor[S](config => path => f(extractor(config, path)))
  }

  abstract class Handler[-K, +H](val Settings: Settings) {
    type From
    def handle(v: From): H
  }

  def apply[T: Extractor](path: String): SimpleSetting[T] = SimpleSetting[T](path)
}

@implicitNotFound("Product does not contain this value")
trait ProductContainsSetting[V, T]

object ProductContainsSetting {
  implicit def acceptable[V, T <: V]: ProductContainsSetting[V, T] = null
}

@implicitNotFound("Coproduct cannot contain this value")
trait CoproductContainsSetting[V, T]

object CoproductContainsSetting {
  implicit def acceptable[V, T <: V]: CoproductContainsSetting[V, T] = null
}

case class Product[T <: Settings](tmap: Map[Settings, Any]) {
  def apply[V <: Settings](value: V)(implicit acc: ProductContainsSetting[V, T]):
      value.Result = tmap(value).asInstanceOf[value.Result]

  override def toString = tmap.map { case (k, v) => s"$k: $v" }.mkString(", ")
}

case class Coproduct[T <: Settings](value: (Settings, Any)) {
  def handle[K, R](handlers: Setting.Handler[K, R]*)(implicit ev: K <:< T): R = {
    val h = handlers.find(_.Settings == value._1).get
    h.handle(value._2.asInstanceOf[h.From])
  }

  override def toString = s"${value._1}: ${value._2}"
}
