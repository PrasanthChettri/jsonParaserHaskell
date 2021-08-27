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
    "languages" : ["Python" , "C++" , "Haskell"] ,
    "Interests" : ["Backend Development" , "ML" , "Functional Programming"] ,
    "Age" : 300
}
```
Output : 
```
Just (JsonObject [("name",JsonString "Prasanth"),("languages",JsonArray [JsonString "Python",JsonString "C++",JsonString "Haskell"]),("Interests",JsonArray [JsonString "Backend Development",JsonString "ML",JsonString "Functional Programming"]),("Age",JsonNumber 300)])
```
