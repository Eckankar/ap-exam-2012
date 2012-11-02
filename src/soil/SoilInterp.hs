--
-- Soil interpreter
-- Exam for Advanced Programming, B1-2012
-- Sebastian Paaske Tørholm <sebbe@diku.dk>
--

module SoilInterp
  ( runProgRR
  , runProgAll
  ) where

import SoilAst
import Control.Monad (void, liftM)
import Data.Maybe (fromMaybe)
import Data.List (find, intercalate, nub, partition)
import Control.Arrow ((***))

--
-- Part 1: Define a name environment
--
data NameEnv = NameEnv [(Name, [Ident])]
    deriving (Show)

-- Functions for insert and lookup

insertName :: NameEnv -> (Name, [Ident]) -> NameEnv
insertName (NameEnv ns) (n,i) =
    case lookup n ns of
        Nothing -> NameEnv $ (n,i) : ns
        Just _  -> error $ "name " ++ n ++ " already exists"

lookupName :: NameEnv -> Name -> [Ident]
lookupName (NameEnv ns) n =
    fromMaybe (error $ "name " ++ n ++ " undefined") (lookup n ns)

--
-- Part 2: Define a function environment
--
data FuncEnv = FuncEnv [(Ident, Func)]
        deriving (Show)

-- Functions for insert and lookup

insertFunc :: FuncEnv -> (Ident, Func) -> FuncEnv
insertFunc (FuncEnv fs) (i,f) =
    case lookup i fs of
        Nothing -> FuncEnv $ (i,f) : fs
        Just _  -> error $ "function " ++ i ++ " already exists"

lookupFunc :: FuncEnv -> Ident -> Maybe Func
lookupFunc (FuncEnv fs) = flip lookup fs

--
-- Part 3: Degine a process and process queue
--
type Message = [Ident]

data Process = Process { procid    :: Ident
                       , function  :: Ident
                       , arguments :: [Ident]
                       , mailbox   :: [Message]
                       }
    deriving (Show)

data ProcessQueue = PQ [Process]
    deriving (Show)

-- Function for process modification
popMessage :: Process -> ProgramEvaluation (Maybe Message)
popMessage p = case mailbox p of
                  []     -> return Nothing
                  (m:mb) ->
                     do replaceProc (procid p) $ p { mailbox = mb }
                        return (Just m)

hasMessages :: Process -> Bool
hasMessages = not . null . mailbox

changeProcFunc :: Process -> Ident -> [Ident] -> Process
changeProcFunc p f args = p { function = f , arguments = args }

getProcFunc :: Process -> (Ident, [Ident])
getProcFunc p = (function p, arguments p)

data ProgramState = PS { funcs :: FuncEnv
                       , procs :: ProcessQueue
                       }
    deriving (Show)

data ProgramEvaluation a = PE {
        runPE :: ProgramState -> (a, ProcessQueue)
    }

instance Monad ProgramEvaluation where
    return a = PE $ \ps -> (a, procs ps)
    pe >>= f = PE $ \ps -> let (a, pq) = runPE pe ps in
                           runPE (f a) $ ps { procs = pq }

instance Functor ProgramEvaluation where
    fmap f = (>>= return . f)

getProcQueue :: ProgramEvaluation ProcessQueue
getProcQueue = PE $ \ps -> let pq = procs ps in (pq, pq)

putProcQueue :: ProcessQueue -> ProgramEvaluation ()
putProcQueue pq = PE $ \_ -> ((), pq)

getFuncEnv :: ProgramEvaluation FuncEnv
getFuncEnv = fmap funcs getProgramState

getProc :: Ident -> ProgramEvaluation (Maybe Process)
getProc pid = do PQ pq <- getProcQueue
                 return $ find ((== pid) . procid) pq

getCurrentProc :: ProcessState -> ProgramEvaluation Process
getCurrentProc ps = let pidm = curPid ps in
                    case pidm of
                       Nothing  -> error "Current process doesn't exist."
                       Just pid ->
                         do pm <- getProc pid
                            case pm of
                               Nothing -> error "Current process doesn't exist."
                               Just p  -> return p

replaceProc :: Ident -> Process -> ProgramEvaluation ()
replaceProc pid p = do PQ pq <- getProcQueue
                       putProcQueue $ PQ $ map (\p' -> if procid p' == pid
                                                       then p
                                                       else p') pq

pushProc :: Process -> ProgramEvaluation ()
pushProc p = do PQ pq <- getProcQueue
                putProcQueue $ PQ $ pq ++ [p]

popProc :: ProgramEvaluation Process
popProc = do PQ pq <- getProcQueue
             case pq of
                []   -> error "Process queue is empty!"
                p:ps -> do putProcQueue $ PQ ps
                           return p

getProgramState :: ProgramEvaluation ProgramState
getProgramState = PE $ \ps -> (ps, procs ps)

sendMessage :: Ident -> Message -> ProgramEvaluation ()
sendMessage pid m =
    do p <- getProc pid
       case p of
           Nothing -> sendMessage "errorlog"
                        ["Cannot send message to nonexistent process #" ++ pid]
           Just proc -> replaceProc pid $ proc { mailbox = mailbox proc ++ [m] }

--
-- Part 4: Define and implement a process step
--
data ProcessState = ProS { nameEnv :: NameEnv
                         , curPid  :: Maybe Ident
                         }
    deriving (Show)

-- use only on values that *must* be names
primToName :: Prim -> Name
primToName Self         = error "Invalid use of self where name expected."
primToName (Id _)       = error "Invalid use of identifier where name expected."
primToName (Concat _ _) = error "Invalid use of concat where name expected."
primToName (Par n)      = n

-- gives [] if one of the identifiers in a concat resolves to 0 or 2+ values
resolveIdent :: ProcessState -> Prim -> [Ident]
resolveIdent _ (Id i)          = [i]
resolveIdent ps Self           = case curPid ps of
                                    Just pid -> [pid]
                                    Nothing  -> error "Self used outside of process."
resolveIdent ps (Par n)        = lookupName (nameEnv ps) n
resolveIdent ps (Concat p1 p2) =
    case (resolveIdent ps p1, resolveIdent ps p2) of
        ([i1], [i2]) -> [i1 ++ i2]
        _        -> []

runActOp :: ProcessState -> ActOp -> ProgramEvaluation ProcessState
runActOp ps aop =
    case aop of
        SendTo msg tgt      ->
            case resolveIdent ps tgt of
                [tpid] -> sendMessage tpid $ concatMap (resolveIdent ps) msg
                _      ->
                   sendMessage "errorlog"
                      [show tgt ++ " resolves to more than one ident in SendTo"]
        Create pidp fidp args ->
            do fe <- getFuncEnv
               case (resolveIdent ps fidp, resolveIdent ps pidp) of
                  ([fid], [pid]) ->
                    case lookupFunc fe fid of
                        Nothing ->
                          sendMessage "errorlog"
                            ["Cannot spawn process for nonexistent function " ++ fid]
                        Just f  ->
                            if length args /= length (params f)
                            then sendMessage "errorlog"
                                 ["Inccorect number of arguments passed to " ++ fid]
                            else let p = Process pid fid
                                            (concatMap (resolveIdent ps) args) [] in
                                 pushProc p
                  _     ->
                    sendMessage "errorlog"
                      ["A name resolves to more than one ident in Create"]
        Become fidp argsp ->
            do p <- getCurrentProc ps
               fe <- getFuncEnv
               case resolveIdent ps fidp of
                [fid] ->
                    let args = concatMap (resolveIdent ps) argsp in
                    case lookupFunc fe fid of
                        Nothing ->
                          sendMessage "errorlog"
                            ["Cannot become nonexistent function " ++ fid]
                        Just f  ->
                            if length args /= length (params f)
                            then sendMessage "errorlog"
                                 ["Inccorect number of arguments passed to " ++ fid]
                            else let p' = changeProcFunc p fid args in
                                 replaceProc (procid p) p'
                _ ->
                    sendMessage "errorlog"
                      ["Function name resolves to more than one ident in Become"]
    >> return ps

runExp :: ProcessState -> Expr -> ProgramEvaluation ()
runExp ps ex =
    let ne = nameEnv ps in
    case ex of
        CaseOf n pts def ->
            let v = lookupName ne $ primToName n in
            case find (\(p,_) -> length p == length v) pts of
                Nothing    -> runExp ps def
                Just (pt,e) ->
                    let ne' = foldl insertName ne $ zip pt $ map (:[]) v in
                    runExp (ps { nameEnv = ne' }) e
        IfEq p1 p2 thexp elexp ->
            let rp1 = resolveIdent ps p1
                rp2 = resolveIdent ps p2 in
            if length rp1 == 1 && length rp2 == 1 && rp1 == rp2
            then runExp ps thexp
            else runExp ps elexp
        Acts aops ->
            void $ foldl (\s a -> s >>= flip runActOp a) (return ps) aops

runFunc :: ProcessState -> Func -> ProgramEvaluation ()
runFunc ps f = runExp ps (body f)

evalFunc :: Ident -> [Ident] -> -- function id, arguments
            Ident -> [Ident] -> -- process id, message contents
            ProgramEvaluation ([String], [String])
evalFunc _ _ "println"  msg = return ([intercalate ":" msg], [])
evalFunc _ _ "errorlog" msg = return ([], [intercalate ":" msg])
evalFunc fid args pid msg =
    do fe <- getFuncEnv
       case lookupFunc fe fid of
           Nothing -> sendMessage "errorlog" ["No such function: " ++ fid] >>
                      return ([], [])
           Just f  -> let ne = NameEnv $ zip (receive f : params f)
                                             (msg : map (:[]) args)
                          ps = ProS ne (Just pid) in
                      runFunc ps f >> return ([],[])

processStep :: Ident -> ProgramEvaluation ([String], [String])
processStep pid =
    do p  <- getProc pid
       case p of
            Nothing -> do sendMessage "errorlog"
                             ["Cannot execute nonexistent process #" ++ pid]
                          return ([], [])
            Just p' -> do mm <- popMessage p'
                          case mm of
                             Nothing -> return ([], [])
                             Just m  -> uncurry evalFunc (getProcFunc p') pid m


--
-- Part 5: Define and implement the roind-robin algorithm
--

-- Not used, though it works fine; nextNonemptyProcessRR used instead
--nextProcessRR :: ProgramEvaluation Ident
--nextProcessRR = do p <- popProc
--                   pushProc p
--                   return $ procid p

nextNonemptyProcessRR :: ProgramEvaluation (Maybe Ident)
nextNonemptyProcessRR =
    do fp <- popProc
       pushProc fp
       if hasMessages fp
       then return $ Just $ procid fp
       else loop $ procid fp
    where loop fpid = do p <- popProc
                         pushProc p
                         if procid p == fpid
                         then return Nothing
                         else if hasMessages p
                         then return $ Just $ procid p
                         else loop fpid

--
-- Part 6: Implement the round-robin evaluator
--
emptyProcessState :: ProcessState
emptyProcessState = ProS (NameEnv []) Nothing

initialProcessQueue :: ProcessQueue
initialProcessQueue = PQ [
        Process "println"  [] [] [],
        Process "errorlog" [] [] []
    ]

compileProgRR :: Int -> ProgramEvaluation ([String], [String])
compileProgRR 0 = return ([], [])
compileProgRR n =
    do pidm <- nextNonemptyProcessRR
       case pidm of
            Nothing  -> return ([], [])
            Just pid -> do (so, se)   <- processStep pid
                           (sos, ses) <- compileProgRR (n-1)
                           return (sos ++ so, ses ++ se)

initialPS :: [Func] -> ProgramState
initialPS fs = let fe = foldl (\e -> insertFunc e . \f -> (funcname f, f))
                              (FuncEnv []) fs in
               PS fe initialProcessQueue

evalInitialActOps :: [ActOp] -> ProgramEvaluation ()
evalInitialActOps aops = runExp emptyProcessState (Acts aops)

runProgRR :: Int -> Program -> ([String], [String])
runProgRR n (fs, aops) =
     fst $ runPE (evalInitialActOps aops >> compileProgRR n)
               $ initialPS fs

--
-- Part 7: Implement a find all possible executions evaluator
--
nextProcAll :: ProgramEvaluation [Ident]
nextProcAll = do PQ pq <- getProcQueue
                 let (nep, ep) = partition hasMessages pq
                 return $ map procid $ case ep of
                    []    -> nep
                    (e:_) -> e:nep


runPid :: Int -> Ident -> ProgramEvaluation [([String], [String])]
runPid n pid = do (so, se) <- processStep pid
                  rems     <- compileProgAll n
                  return $ map ((++ so) *** (++ se)) rems

compileProgAll :: Int -> ProgramEvaluation [([String], [String])]
compileProgAll 0 = return [([],[])]
compileProgAll n = do pids <- nextProcAll
                      pq   <- getProcQueue
                      let curState = putProcQueue pq
                      liftM concat $ mapM ((curState >>) . runPid (n-1)) pids

runProgAll :: Int -> Program -> [([String], [String])]
runProgAll n (fs, aops) =
    nub $ fst $ runPE (evalInitialActOps aops >> compileProgAll n)
                    $ initialPS fs
