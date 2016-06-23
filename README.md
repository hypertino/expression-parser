# expession-parser

Parses and tests if expression is true

## Supported operations:
"!, =, !=, and, or" - standard boolean operations 
"someObject.someField" - evaluate value of 'someField' of 'someObject'
"has, has not" - tests if collection has (or has not) specified element 
"in, not in": 
1. tests whether element is present (or missing) in collection
2. tests whether ip address matches (or not) ip range

## Values representation:
1. Values of fields of some object should be dot-separated:
```scala
user.field.subField
```

2. Strings can be written with or without quotes of apostrophes:
```scala
someString
'someString'
"someString"
```

3. Numbers should be written as is, without any additional symbols
4. IP addresses should be written in regular way (octets, separated by dots), IP range should contain lower bound IP, `-` or ` - ` as separator and upper bound IP address. Can be in parenthesis:
```scala
10.10.10.10
10.10.0.0 - 10.10.9.9
10.10.0.0-10.10.9.9
(10.10.0.0 - 10.10.9.9)
```

## Examples:
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

10.10.10.10 not in 10.10.0.0 - 10.10.9.9
10.10.10.10 in 10.10.0.0 - 10.10.20.20
user.ip in 10.10.0.0 - 10.10.20.20
qa in user.roles
admin not in user.roles
'admin' not in user.roles
"admin" not in user.roles
```

## Restrictions:
1. Currently left and right subexpressions of `and` and `or` operators should be in parenthesis
2. Evaluation of objects' fields values doesn't use reflection for now, so you should use MapBasedEvaluationEngine with provided map of type Map[String, Any], which contains fields' values. 
For example:
```scala
val evalEngine = new MapBasedEvaluationEngine(Map(
  "user" → Map(
    "isDefined" → true,
    "roles" → Seq("dev", "qa", "ops")
  )
))
```