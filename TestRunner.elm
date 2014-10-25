module Main where

import Test
import ElmTest.Run as Run
import ElmTest.Runner.Element as Element
import ElmTest.Runner.String  as String

prettyOut : Element
prettyOut = Element.runDisplay Test.suite

main : Element
main = flow down [ plainText "\nThe Element runner:\n\n"
                 , prettyOut
                 ]