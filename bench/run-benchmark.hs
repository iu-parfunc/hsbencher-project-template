{-# LANGUAGE NamedFieldPuns #-}

-- | HSBencher script to run all the benchmarks.

module Main where

import HSBencher
import HSBencher.Backend.Fusion    (defaultFusionPlugin)
-- import HSBencher.Backend.Codespeed (defaultCodespeedPlugin, CodespeedConfig(..))
import HSBencher.Backend.Dribble   (defaultDribblePlugin)
import HSBencher.Methods.Builtin   (makeMethod)

import qualified Control.Monad.Trans as Trans

import Data.Default (Default(def))
import Data.Monoid (mappend)
import qualified Data.Map as M
import System.Environment (getEnvironment)
import System.Directory   (setCurrentDirectory, getDirectoryContents, getCurrentDirectory)
import System.IO.Unsafe   (unsafePerformIO)
import System.Process     
import GHC.Conc           (getNumProcessors)

--------------------------------------------------------------------------------

benches :: [Benchmark DefaultParamMeaning]
benches = 
-- [ shellBenchmark "racket infer-timing.rkt" ("--hsbencher" : words args) (And [])
  -- Each call to "  
  [ (mkBenchmark "../hm-typing/hm-typing.cabal" [] (hmInferParams))
    { progname = Just "hm-typing" }
--  , mkBenchmark "TRinfer-bigcall" []   (trInferParams [4 .. 7] [""])
  , mkBenchmark "TRinfer-bigcall" []   (trInferParams [3,4] [""])
  , (mkBenchmark "TRinfer-treecall" [] (trInferParams [14,15] [""]))
    { progname= Just "TRinfer-treecall-noflip"}
  , (mkBenchmark "TRinfer-treecall" [] (trInferParams [14,15] [" flipped"]))
    { progname= Just "TRinfer-treecall-flipped"} 
  , (mkBenchmark "infer-timing.rkt" []
                (Or [ Set (Variant "racket") (RuntimeArg$ unwords ["--", "--hsbencher", size, "bigcall"])
                    | size <- map show [1 .. 5::Int] ]))
    { progname = Just "bigcall-racket" }
  , (mkBenchmark "infer-timing.rkt" []
                (Or [ Set (Variant "racket") (RuntimeArg$ unwords ["--", "--hsbencher", size, "treecall"])
                    | size <- map show [1 .. 10::Int] ]))
    { progname = Just "treecall-racket" }    
  , mkBenchmark "BoolVarTree" []
    (And [haskell_rts_opts
         , Or $ [ Set NoMeaning (RuntimeArg$ unwords [ver, size, assignments])
              | ver <- ["naiveSucceed", "naiveFail", "lvish1Succeed", "lvish1Fail", "lvish3Succeed", "lvish3Fail"]
              , size <- map show boolVarTreeSizes
              , assignments <- map (show . (10^)) [1 .. 3]
              ] ++
              [ -- unbalanced trees for layered maps
                Set NoMeaning (RuntimeArg $ unwords [ver, "0", assignments, "True"])
              | ver <- ["naiveSucceed", "naiveFail", "lvish3Succeed", "lvish3Fail"]
              , assignments <- map (show . (10^)) [1 .. 3]
              ]])
  -- [
  --   (mkBenchmark "./BoolVarTree" [ver, "0", assignments, "True"] (And [haskell_rts_opts]))
  --    { progname = Just "BoolVarTree-unbalanced" }
  --    | ver <- ["naiveSucceed", "naiveFail",  "lvish3Succeed", "lvish3Fail"] -- lvish2 can't do arbitrary depth
  --    , assignments <- map (show . (10^)) [1..3]
  -- ]
  -- [
  --   mkBenchmark ("boolvar-" ++ succeed ++ "-" ++ size ++ "-" ++ assignments ++ ".pl") [] (And [])
  --   | succeed <- ["True", "False"]
  --   , size <- map show boolVarTreeSizes
  --   , assignments <- map (show . (10^)) [1..3]
  -- ]
--  For later: we can use the Makefile method.  This will be better if we add CompileEnv:

  , (mkBenchmark ("BoolVarTree/prolog/Makefile") []
     (Or [ And [ Set NoMeaning (CompileParam (unwords [succeed, size, assignments]))
               , Set NoMeaning (RuntimeArg (unwords [succeed, size, assignments])) ]
         | succeed <- ["True", "False"]
         , size <- map show boolVarTreeSizes
         , assignments <- map (show . (10^)) [1..3] ]))
    { progname = Just "boolvartree-prolog", overrideMethod=Just makeMethod }
  ]

--------------------------------------------------------------------------------
-- Param settings and sizes:
--------------------------------------------------------------------------------

boolVarTreeSizes :: [Int]
boolVarTreeSizes = map (10^) [2 .. 4]

hmInferParams :: BenchSpace DefaultParamMeaning
hmInferParams =
  And [ haskell_rts_opts
      ,  Or [ fn (Set (Variant var) (RuntimeArg (var++" "++bench++" "++show sz')))
            | (var,fn) <- [("HMSeq",id), ("HMPar",varyThreads)]
            , (bench, sz) <- ls1
            , sz'  <- take 3 $ iterate (+1) sz]]
 where
  ls1 = [("embParHetero",19), ("embParHetero",20)] 
  ls2 = [("bigTerm", 22), ("bigType", 16),
         ("fanOutTreePoly", 17), ("fanOutListPoly", 17),
         ("fanOutListMono", 20), ("fanOutTreeMono", 20)]

  
trInferParams :: [Int] -> [String] -> BenchSpace DefaultParamMeaning
trInferParams szs  extras =
  Or [ And [ haskell_rts_opts
           , Or ((varyImpl sz extra
                    [ ("pure","RESULT_PURE")
--                    , ("pure","RESULT_PURE_SORTED")
                    , ("layered","LAYERED")
                    , ("satmap","SATMAP")
                    ]) -- TODO: Vary filtset/Pureset
                  ++
                 [ And [ Set (Variant ("seq_"++mode)) (RuntimeArg ("seq "++sz ++extra))
                       , Set NoMeaning                (RuntimeEnv "INFERWHICH" mode)
                       ]
                 | mode <- ["first","all"]]
                )
           ]
     | sz <- map show szs
     , extra <- extras ]
-- TODO: Generalize this:
--   vary "par"
--     [ (["LAYERED","SATMAP"], (\s -> CompileParam ("--ghc-option=-DUSE_"++s)))
--     , (["first","all","any"], (\s -> RuntimeEnv "INFERWHICH" s)) ]  
 where
   varyImpl sz extra ls =
    [ varyThreads $ 
      And [ Set (Variant$ "par_"++ormode++"_"++andmode++"_"++name++"_all")
                (RuntimeArg ("par "++sz++extra))
          , Set NoMeaning                       (RuntimeEnv "INFERWHICH" "all")
          , Set NoMeaning                       (CompileParam$ "--ghc-option=-DUSE_"++sym)
          , Set NoMeaning                       (CompileParam$ andflg)
          , Set NoMeaning                       (CompileParam$ orflg)
          ]
    | (name,sym) <- ls
    , (andmode,andflg) <- [("parAnd","--ghc-option=-DUSE_AND_PAR"),("seqAnd","")]
    , (ormode, orflg)  <- [("parOr", "--ghc-option=-DUSE_OR_PAR"), ("seqOr","")]
    ]

  
haskell_rts_opts :: BenchSpace DefaultParamMeaning
haskell_rts_opts = Set NoMeaning (RuntimeParam "+RTS -s -qa -RTS")


--------------------------------------------------------------------------------
-- Helpers:

-- | GHC specific method of varying threads.
varyThreads :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
varyThreads conf = And [ conf, Or (map fn threadSelection) ]
 where
   fn n = Set (Threads n) $ RuntimeParam ("+RTS -N"++ show n++" -RTS")

threadSelection :: [Int]
threadSelection = unsafePerformIO $ do
  env <- getEnvironment
  p0  <- getNumProcessors
  let p = case lookup "MAXREALCORES" env of
            Nothing -> p0
            Just p' -> case reads p' of
                         ((x,_):_) -> min p0 x
                         [] -> error $"run-benchmark: could not parse MAXREALCORES as Int: "++show p'
  return$
    if p <= 8  then [1..p] else
    if p <= 16 then 1: [2,4 .. p]
    else            1:2:[4,8 .. p]

--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Begin Racket type-checking benchmarks..."
  -- putStrLn$ "Automatic thread selection: "++show threadSelection
  defaultMainModifyConfig $ \ conf ->
    addPlugin defaultFusionPlugin fsconf $ 
    addPlugin defaultDribblePlugin def $ 
--    addPlugin defaultCodespeedPlugin csconf $ 
    conf{ benchlist  = benches
          -- Why was this 1000? [2014.10.29] 
--        , runTimeOut = Just 1000 -- Erk... need a separate compile timeout.
        , runTimeOut = Just 120
        , buildMethods = racketMethod : buildMethods conf
        , harvesters = customTagHarvesterInt "NUMRESULTS" `mappend` 
                       harvesters conf
        }
 where
  -- Some default settings for benchmarking:
  -- csconf = def { codespeedURL = "http://codespeed.crest.iu.edu"
  --              , projName     = "LVishParInfer" }

  -- By default we can bake settings in here rather than passing them
  -- on the command line:
  fsconf = def 
-- CID=820162629229-kp29aklebt6ucos5a71u8tu3hu8unres.apps.googleusercontent.com
-- SEC=pSsMxVAJCFKyWsazuxZVRZwX

-- | Teach HSBencher how to run racket files.
racketMethod :: BuildMethod
racketMethod = BuildMethod
  { methodName = "racket"
  , canBuild = WithExtension ".rkt"
  , concurrentBuild = False
  , setThreads      = Nothing
  , clean = \ pathMap _ target -> return ()
  , compile = \ pathMap bldid flags target -> do
       _ <- Trans.lift $ system "raco pkg install gcstats"
       let runit args envVars =
             CommandDescr
             { command = ShellCommand ("racket -l gcstats -t "++target++" "++ unwords args )
             , timeout = Just 100
             , workingDir = Nothing
             , envVars
             , tolerateError = False
             }
       return (RunInPlace runit)
  }

-- -- | Check for a SELFTIMED line of output.
-- compilerHarvester :: LineHarvester
-- compilerHarvester = taggedLineHarvester "COMPILER" (\ d r -> r{???=d})
