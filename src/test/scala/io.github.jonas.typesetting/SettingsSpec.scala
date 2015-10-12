/*
 * Copyright 2015 Jonas Fonseca
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

import java.util.NoSuchElementException
import scala.concurrent.duration._
import scala.util.{ Try, Success, Failure }
import com.typesafe.config.{ Config, ConfigFactory, ConfigException }
import org.specs2._

case class Auth(uid: Long, pass: String)
object Auth {
  implicit val authExtractor: Setting.Extractor[Auth] =
    implicitly[Setting.Extractor[String]].map { value =>
      val Array(uid, pass) = value.split(":")
      Auth(uid.toLong, pass)
    }
}

class SettingSpec extends org.specs2.mutable.Specification {

  val config = ConfigFactory.parseString("""
    boolean-setting = true
    int-setting = 42
    long-setting = 7357
    number-setting = 987654321
    double-setting = 3.14
    double-list-setting = [ -3.14, 0, 3.14 ]
    string-setting = hello world
    string-list-setting = [ hello,  world ]
    memory-size-setting = 1 k
    duration-setting = 2 minutes

    failure-int-setting = not an int
    custom-setting = "1234:p4ss"
  """)

  val BooleanSetting = Setting[Boolean]("boolean-setting")
  val IntSetting = Setting[Int]("int-setting")
  val LongSetting = Setting[Long]("long-setting")
  val NumberSetting = Setting[Number]("number-setting")
  val DoubleSetting = Setting[Double]("double-setting")
  val DoubleListSetting = Setting[Seq[Double]]("double-list-setting")
  val StringSetting = Setting[String]("string-setting")
  val StringListSetting = Setting[Seq[String]]("string-list-setting")
  val MemorySizeSetting = Setting[Setting.MemorySize]("memory-size-setting")
  val DurationSetting = Setting[Duration]("duration-setting")

  val FailureIntSetting = Setting[Try[Int]]("failure-int-setting")
  val SuccessIntSetting = Setting[Try[Int]]("int-setting")

  val NoneStringSetting = Setting[Option[String]]("maybe-string-setting")
  val SomeStringSetting = Setting[Option[String]]("string-setting")
  val NotAStringSetting = Setting[String]("not-a-string-setting")

  val CustomSetting = Setting[Auth]("custom-setting")

  val AllSettings =
    BooleanSetting & IntSetting & LongSetting &
      NumberSetting & DoubleSetting & DoubleListSetting &
      StringSetting & StringListSetting & MemorySizeSetting &
      DurationSetting & FailureIntSetting & SuccessIntSetting &
      NoneStringSetting & SomeStringSetting & CustomSetting

  "Setting" >> {
    "singletons" >> {
      BooleanSetting.parse(config) must_== true
      IntSetting.parse(config) must_== 42
      LongSetting.parse(config) must_== 7357
      NumberSetting.parse(config) must_== 987654321
      DoubleSetting.parse(config) must_== 3.14
      DoubleListSetting.parse(config) must_== Seq(-3.14, 0.0, 3.14)
      StringSetting.parse(config) must_== "hello world"
      StringListSetting.parse(config) must_== Seq("hello", "world")
      MemorySizeSetting.parse(config) must_== Setting.MemorySize(1024)
      DurationSetting.parse(config) must_== 2.minutes
      FailureIntSetting.parse(config) must haveClass[Failure[ConfigException.WrongType]]
      SuccessIntSetting.parse(config) must_== Success(42)
      NoneStringSetting.parse(config) must_== None
      SomeStringSetting.parse(config) must_== Some("hello world")
      CustomSetting.parse(config) must_== Auth(1234, "p4ss")

      { NotAStringSetting.parse(config) } must throwA[ConfigException.Missing]
    }

    "products" >> {
      val settings = AllSettings.parse(config)

      settings(BooleanSetting) must_== true
      settings(IntSetting) must_== 42
      settings(LongSetting) must_== 7357
      settings(NumberSetting) must_== 987654321
      settings(DoubleSetting) must_== 3.14
      settings(DoubleListSetting) must_== Seq(-3.14, 0.0, 3.14)
      settings(StringSetting) must_== "hello world"
      settings(StringListSetting) must_== Seq("hello", "world")
      settings(MemorySizeSetting) must_== Setting.MemorySize(1024)
      settings(DurationSetting) must_== 2.minutes
      settings(FailureIntSetting) must haveClass[Failure[ConfigException.WrongType]]
      settings(SuccessIntSetting) must_== Success(42)
      settings(NoneStringSetting) must_== None
      settings(SomeStringSetting) must_== Some("hello world")
      settings(CustomSetting) must_== Auth(1234, "p4ss")

      { settings(NotAStringSetting) } must throwA[NoSuchElementException]
    }
  }
}
