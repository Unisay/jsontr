JsonTr
======
[![Build Status](https://travis-ci.org/Unisay/jsontr.svg?branch=master)](https://travis-ci.org/Unisay/jsontr)

JsonTr is a JSON-based declarative transformation language for JSON (like XSLT for XML).

Given source JSON
```json
{
   "sup":{
      "sub":{
         "name":"middle"
      }
   }
}
```
and template JSON
```json
{
  "~match /sup": [
    "before",
    "~value-of sub/name",
    "after"
  ]
}
```
result of transformation will be 
```json
[
  "before",
  "middle", 
  "after" 
]
```

The project is under development and is not yet ready for use.
