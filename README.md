[![Build Status](https://travis-ci.org/hypertino/expression-parser.svg)](https://travis-ci.org/hypertino/expression-parser)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.hypertino/expression-parser_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.hypertino/expression-parser_2.13)

[ Latest releases and snapshots](https://oss.sonatype.org/#nexus-search;gav~com.hypertino~expression-parser_*~~~)

# About
This is a ready to use expression parser and evaluator for Scala powered by `parboiled2`.

Example:

```scala
val result = HEval("4*5+3*2").toInt
// result = 26
```

It's possible to provide a context with variables to the parser:

```scala
val context = Obj.from("user" -> Obj.from("count" -> 10))
val result = HEval("4*user.count+1*2", context).toInt
// result = 42
```

You also can add your own functions into the context:

```scala
val context = new ValueContext(Obj.empty) {
    override def function: PartialFunction[(Identifier, Seq[Value]), Value] = {
        case (Identifier(Seq("pow")), args) => args.head.toBigDecimal.pow(args.tail.head.toInt)
    }
}

val result = HEval("pow(2,8)", context).toInt
// result = 256
```

## TODO
 + HParser function

## Built-in operators 
 `+, -, *, /` - arithmetical operations
 
 `!, =, !=, and, or, xor` - boolean operations
 
 `%` - remainder (modulus) operation
 
 `>, <, >=, <=` - comparison operations
 
 `someObject.someField` - evaluate value of 'someField' of 'someObject'
 
 `has, has not` - tests if collection has (or has not) specified element 
 
 `like, not like` - tests if value matches regexp
 
 `++` - adds one collection to another
 
 `--` - computes difference between two collections

## Built-in functions
 `case` - takes two arguments: index and collection. Returns value placed in collection with specified index
 
 `isEmpty` - tests whether value is empty
 
 `isExists` - tests whether value is present in context
 
 `length` - returns length of collection or string. Boolean argument is a special case: function returns '1' in case of 'true' and '0' in case of 'false'
 
 `upper, lower` - returns string in upper or lower case
 
 `split` - takes two arguments: string and separator and returns collection made by splitting of incoming string by specified separator
 
 `indexOf` - takes two arguments: string and substring. Returns the index within this string of the first occurrence of the specified substring
 
 `substr` - takes 2 or 3 arguments: string, beginIndex[, endIndex] - returns substring of string
 
 `compareIgnoreCase` - compares two strings ignoring their case

## Context field access
 1. Values of fields of some object should be dot-separated:
 ```scala
 user.field.subField
 ```

 2. Strings can be written with quotes of apostrophes (later ignores escapes):
 ```scala
 'someString\nabcde' // escaping ignored
 "someString\nSecondLine" // newline inserted
 ```

 3. Numbers should be written as is, without any additional symbols

## Other Examples (TODO)
 ```scala
 user.isDefined
 !user.isDefined
 user.isDefined = true 
 user.isDefined != true
 user.roles has "qa"
 user.roles has not "admin"
 (user.isDefined = true) and (user.roles has "qa") 
 (user.isDefined) and (user.roles has "qa")) 
 (!user.isDefined) or (user.roles has "admin")
 ```

# Download

```sbt
libraryDependencies += "com.hypertino" %% "expression-parser" % "0.3.0"
```
Releases published to Maven Central for Scala 2.11 - 2.13 JVM & JS (user `%%%` for Scala.js enabled projects)

Snapshots live in Sonatype repository, include it additionally:
```sbt
resolvers ++= Seq(
  Resolver.sonatypeRepo("public")
)
```

# License

library is available under the BSD 3-Clause License