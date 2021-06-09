**A sample json parser from scratch in Haskell**

* Using Parser combinartors
* Features : Completely built from scratch without third party dependencies
* TODO :
    * Support escape statements for string parse
    * Support Floating points for numbers 

Sample Input : 
```
{
    "name" : "Prasanth" ,
    "languages" : ["Python" , "Haskell" , "APL", "Lisp"] ,
    "Interests" : ["Backend Development" , "DAPP" , "Functional Programming"] ,
    "Age" : 300
}
```
Output : 
```
Just (JsonObject [("name",JsonString "Prasanth"),("languages",JsonArray [JsonString "Python",JsonString "Haskell",JsonString "APL",JsonString "Lisp"]),("Interests",JsonArray [JsonString "Backend Development",JsonString "DAPP",JsonString "Functional Programming"]),("Age",JsonNumber 300)])
```
