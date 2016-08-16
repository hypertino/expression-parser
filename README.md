# expression-parser
 Parses and evaluates expression

## TODO
 + HParser function

## Supported operations: 
 "+, -, *, /" - standard arithmetical operations
 "!, =, !=, and, or, xor" - standard boolean operations
 "%" - remainder (modulus) operation
 ">, <, >=, <=" - comparison operations
 "someObject.someField" - evaluate value of 'someField' of 'someObject'
 "has, has not" - tests if collection has (or has not) specified element 
 "like, not like" - tests if value matches regexp
 "++" - adds one collection to another
 "--" - computes difference between two collections

## Supported functions:
 "case" - takes two arguments: index and collection. Returns value placed in collection with specified index
 "isEmpty" - tests whether value is empty
 "isExists" - tests whether value is present in context
 "length" - returns length of collection or string. Boolean argument is a special case: function returns '1' in case of 'true' and '0' in case of 'false'
 "upper, lower" - returns string in upper or lower case
 "split" - takes two arguments: string and separator and returns collection made by splitting of incoming string by specified separator
 "indexOf" - takes two arguments: string and substring. Returns the index within this string of the first occurrence of the specified substring
 "substr" - takes 2 or 3 arguments: string, beginIndex[, endIndex] - returns substring of string
 "compareIgnoreCase" - compares two strings ignoring their case

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
 ```