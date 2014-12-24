module Test.Migration.ScratchTest (suite) where

import ElmTest.Assertion (..)
import ElmTest.Test (..)
import Json.Decode
import Outline.Scratch.Json as Scratch
import Core.Array
import Result

v1 =
  ( """["Scratch 3\\n\\nBlah","Scratch 2"]"""
  , """[[{"type":"Paragraph","value":[{"type":"Normal","value":"Scratch 3"}]},{"type":"Paragraph","value":[{"type":"Normal","value":""}]},{"type":"Paragraph","value":[{"type":"Normal","value":"Blah"}]}],[{"type":"Paragraph","value":[{"type":"Normal","value":"Scratch 2"}]}]]"""
  )

v2 =
  ( """[[{"type":"Heading","value":[{"type":"Normal","value":"Scratch 1"}]},{"type":"Paragraph","value":[{"type":"Normal","value":"Text"}]}]]"""
  , """[[{"type":"Heading","value":[{"type":"Normal","value":"Scratch 1"}]},{"type":"Paragraph","value":[{"type":"Normal","value":"Text"}]}]]"""
  )

migrate : String -> Result String String
migrate input = input
  |> Json.Decode.decodeString Scratch.listDecoder
  |> Result.map (Core.Array.toJson Scratch.toJson)

testMigration : String -> (String, String) -> Test
testMigration name (inputJson, migratedJson)
  = test name <| migrate inputJson `assertEqual` Ok migratedJson

suite = Suite "JSON migration: Scratch"
  [ testMigration "v1" v1
  , testMigration "v2" v2
  ]
