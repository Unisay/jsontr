jsontr
======

JsonTr is a transformation language for JSON (like XSLT for XML).

For example:
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
  "(match /sup)": [
    "before",
    "(value-of sub/name)",
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



[![Build Status](https://travis-ci.org/Unisay/jsontr.svg?branch=master)](https://travis-ci.org/Unisay/jsontr)
