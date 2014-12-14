module Main where

import IO.IO (..)
import IO.Runner (Request, Response, run)

import ElmTest.Runner.Console (runDisplay)

import Tests

testRunner : IO ()
testRunner = runDisplay Tests.all

port requests : Signal Request
port requests = run responses testRunner

port responses : Signal Response
