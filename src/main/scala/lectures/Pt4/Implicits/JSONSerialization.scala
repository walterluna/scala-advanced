package lectures.Pt4.Implicits

import java.util.Date

object JSONSerialization extends App {

  case class User(name: String, age: Int, email: String)

  case class Post(content: String, createdAt: Date)

  case class Feed(user: User, posts: List[Post])

  /*
   1 - intermediate data types: Int, String, List, Date
   2 - type classes for conversion to intermediate data types
   3 - serialize to JSON
  */

  //  1 - intermediate data types: Int, String, List, Date
  sealed trait JSONValue {
    def stringify: String
  }

  final case class JSONString(value: String) extends JSONValue {
    override def stringify: String = "\"" + value + "\""
  }

  final case class JSONNumber(value: Int) extends JSONValue {
    override def stringify: String = value.toString
  }

  final case class JSONArray(values: List[JSONValue]) extends JSONValue {
    override def stringify: String = values.map(_.stringify).mkString("[", ", ", "]")
  }

  final case class JSONObject(values: Map[String, JSONValue]) extends JSONValue {
    override def stringify: String = values.map {
      case (key, value) => "\"" + key + "\"" + ": " + value.stringify
    }
      .mkString("{", ", ", "}")
  }

  val data = JSONObject(Map(
    "user" -> JSONString("Walter"),
    "posts" -> JSONArray(List(
      JSONString("Scala Rocks!"),
      JSONNumber(453),
    )),
  ))

  println(data.stringify)

  // 2 - type classes for conversion to intermediate data types
  /*
  1 - type class
  2 - type class instances
  3 - pimp my library
  */
  //  2.1
  trait JSONConverter[T] {
    def convert(value: T): JSONValue
  }

  // 2.3
  implicit class JSONOps[T](value: T) {
    def toJson(implicit converter: JSONConverter[T]): JSONValue =
      converter.convert(value)
  }

  //  2.2
  implicit object StringConverter extends JSONConverter[String] {
    override def convert(value: String): JSONValue = JSONString(value)
  }

  implicit object NumberConverter extends JSONConverter[Int] {
    override def convert(value: Int): JSONValue = JSONNumber(value)
  }

  implicit object UserConverter extends JSONConverter[User] {
    override def convert(user: User): JSONValue = JSONObject(Map(
      "name" -> JSONString(user.name),
      "age" -> JSONNumber(user.age),
      "email" -> JSONString(user.email),
    ))
  }

  implicit object PostConverter extends JSONConverter[Post] {
    override def convert(post: Post): JSONValue = JSONObject(Map(
      "content" -> JSONString(post.content),
      "createdAt" -> JSONString(post.createdAt.toString)
    ))
  }

  implicit object FeedConverter extends JSONConverter[Feed] {
    override def convert(feed: Feed): JSONValue = JSONObject(Map(
      "user" -> (feed.user.toJson),
      "post" -> JSONArray(feed.posts.map(_.toJson)),
    ))
  }

  //  3 - serialize to JSON
  val now = new Date(System.currentTimeMillis())
  val john = User("John", 55, "john@mail.com")
  val feed = Feed(john, List(
    Post("hello", now),
    Post("goodbye", now),
  ))

  println(feed.toJson.stringify)
  //  toJson converts to intermediate representation
  //  stringify converts IM to JSON string
}
