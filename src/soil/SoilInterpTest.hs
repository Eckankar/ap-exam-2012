import SoilParser
import SoilInterp
import Test.HUnit
import Data.List (isPrefixOf)

helloWorldTest :: IO Test
helloWorldTest =
    do Right program <- parseFile "examples/helloWorld.soil"
       return $ TestList $ map TestCase [
           assertEqual "hw: run 0 steps = empty output" ([], [])
                     $ runProgRR 0 program,
           assertEqual "hw: run 2 steps = empty output" ([], [])
                     $ runProgRR 2 program,
           assertEqual "hw: run 3 steps = hello printed" (["Hello"], [])
                     $ runProgRR 3 program,
           assertEqual "hw: run 4 step = hello world printed" (["Hello", "World"], [])
                     $ runProgRR 4 program,
           assertEqual "hw: run all 0 steps = empty output" [([],[])]
                     $ runProgAll 0 program,
           assertEqual "hw: run all 1 step = empty output" [([],[])]
                     $ runProgAll 1 program,
           assertEqual "hw: run all 2 steps" [([],[]), (["Hello"],[]), (["World"],[])]
                     $ runProgAll 2 program,
           assertEqual "hw: run all 3 steps" [([],[]), (["Hello"],[]), (["World"],[])]
                     $ runProgAll 3 program,
           assertEqual "hw: run all 4 steps" [([],[]), (["Hello"],[]), (["World"],[]),
                                              (["Hello", "World"],[]),(["World", "Hello"],[])]
                     $ runProgAll 4 program,
           assertEqual "hw: run all 7 steps" [([],[]), (["Hello"],[]), (["World"],[]),
                                              (["Hello", "World"],[]),(["World", "Hello"],[])]
                     $ runProgAll 7 program
        ]

gateKeeperTest :: IO Test
gateKeeperTest =
    do Right program <- parseFile "examples/gateKeeper.soil"
       return $ TestList $ map TestCase [
            assertEqual "gk: run 0 steps = empty output" ([], [])
                $ runProgRR 0 program,
            assertEqual "gk: run 20 steps (output provided by Troels the oracle)"
                (["Hello:World"], ["Cannot send message to nonexistent process #foofoo"])
                $ runProgRR 20 program,
            assertBool "gk: run 250 steps"
                (let (o, e) = runProgRR 250 program in
                  all (== "Hello:World") o &&
                  all ("Cannot send message to nonexistent process" `isPrefixOf`) e)
        ]

cleanUpTest :: IO Test
cleanUpTest =
    do Right program <- parseFile "examples/cleanUp.soil"
       return $ TestList $ map TestCase [
            assertEqual "cleanUp: run 0 steps = empty output" ([], [])
                $ runProgRR 0 program,
            assertBool "gk: run 250 steps"
                (let (o, e) = runProgRR 250 program in
                  all (== "SkippingMessage") o && null e)
        ]

tests :: IO Test
tests = fmap TestList $ sequence [helloWorldTest, gateKeeperTest, cleanUpTest]

main :: IO Counts
main = do t <- tests
          runTestTT t
