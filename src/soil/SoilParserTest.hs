import SoilParser
import SoilAst
import Test.HUnit
import System.FilePath (replaceExtension, replaceBaseName)

-- ASTs {{{
helloWorldAST :: Program
helloWorldAST =
    (
        [Func {
            funcname = "print",
            params = [],
            receive = "message",
            body = Acts [SendTo [Par "message"] (Id "println")]}
        ],
        [Create (Id "hw1") (Id "print") []
        ,Create (Id "hw2") (Id "print") []
        ,SendTo [Id "Hello"] (Id "hw1")
        ,SendTo [Id "World"] (Id "hw2")
        ]
    )

cleanUpAST :: Program
cleanUpAST =
    (
        [Func {
            funcname = "dub",
            params = [],
            receive = "message",
            body = CaseOf (Par "message") [
                (["sender", "msg"]
                    ,Acts [
                        SendTo [Self, Par "msg"] (Par "sender"),
                        SendTo [Self, Par "msg"] (Par "sender")]
                )]
                (Acts [SendTo [Id "FaultyMessage"] (Id "println")])
            }
        ,Func {
            funcname = "half",
            params = ["state"],
            receive = "message",
            body = IfEq (Par "state") (Id "skip")
                (Acts [
                        Become (Id "half") [Id "return"],
                        SendTo [Id "SkippingMessage"] (Id "println")]
                )
                (CaseOf (Par "message") [
                    (["sender", "msg"]
                        ,Acts [
                            Become (Id "half") [Id "skip"],
                            SendTo [Self, Par "msg"] (Par "sender")]
                    )]
                    (Acts [SendTo [Id "FaultyMessage"] (Id "println")])
                )
            }
        ],
        [Create (Id "dubproc") (Id "dub") []
        ,Create (Id "halfproc") (Id "half") [Id "return"]
        ,SendTo [Id "halfproc", Id "foo"] (Id "dubproc")
        ]
    )

gateKeeperAST :: Program
gateKeeperAST =
    (
        [Func {
            funcname = "printer",
            params = [],
            receive = "message",
            body = Acts [SendTo [Par "message"] (Id "println")]
            }
        ,Func {
            funcname = "gate",
            params = ["fst", "fstmsg"],
            receive = "message",
            body = CaseOf (Par "message") [
                (["snd", "sndmsg"]
                    , IfEq (Par "fst") (Id "none")
                        (Acts [Become (Id "gate") [Par "snd", Par "sndmsg"]])
                        (Acts [
                            SendTo [Par "fstmsg", Par "sndmsg"]
                                (Concat (Par "fst") (Par "snd"))
                            ,
                            Become (Id "gate") [Id "none", Id "none"]]
                        )
                )]
                (Acts [])
            }
        ,Func {
            funcname = "repeat",
            params = ["other"],
            receive = "message",
            body = Acts [
                SendTo [Par "message"] (Id "gatekeeper"),
                SendTo [Par "message"] (Par "other")]
            }
        ],
        [Create (Id "foobar") (Id "printer") []
        ,Create (Id "gatekeeper") (Id "gate") [Id "none", Id "none"]
        ,Create (Id "repeater1") (Id "repeat") [Id "repeater2"]
        ,Create (Id "repeater2") (Id "repeat") [Id "repeater1"]
        ,SendTo [Id "foo", Id "Hello"] (Id "repeater1")
        ,SendTo [Id "bar", Id "World"] (Id "repeater2")
        ,SendTo [Id "foo", Id "Bye"] (Id "repeater1")
        ]
    )
-- }}} end of ASTs

-- Test cases that were handed out
fileTestCases :: [(String, Program)]
fileTestCases = [("helloWorld", helloWorldAST),
             ("cleanUp", cleanUpAST),
             ("gateKeeper", gateKeeperAST)]

testWithFile :: (String, Program) -> [IO Test]
testWithFile (f, ast) =
    [ do got <- parseFile filePath
         return $ TestCase $ assertEqual ("parseFile (" ++ f ++ ")")
                                         (Right ast) got
    , do src <- readFile filePath
         return $ TestCase $ assertEqual ("parseString (" ++ f ++ ")")
                                         (Right ast) $ parseString src
    ]
    where file = replaceExtension f ".soil"
          filePath = replaceBaseName "examples/" file

-- Does an invalid program give the correct Error?
invalidProgramTest :: IO Test
invalidProgramTest = return $ TestCase
                            $ assertEqual "invalid program"
                              (Left InvalidProgram)
                            $ parseString "case foo of _ : end"

-- Is concat left-associative?
concatAssocTest :: IO Test
concatAssocTest = return $ TestCase
                         $ assertEqual "concat associativity"
                           (Right ([], [SendTo [] (Concat (Concat (Par "a") (Par "b")) (Par "c"))]))
                         $ parseString "send () to a concat b concat c"

fileTests :: IO Test
fileTests = fmap TestList $ sequence $ concatMap testWithFile fileTestCases

tests :: IO Test
tests = fmap TestList $ sequence [fileTests, invalidProgramTest, concatAssocTest]

main :: IO Counts
main = do t <- tests
          runTestTT t
