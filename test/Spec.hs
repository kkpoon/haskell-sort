{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Sort (quicksort, mergesort)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs


specs :: Spec
specs = describe "sort" $ do
    describe "quicksort" $ for_ cases testQuicksort
    describe "mergesort" $ for_ cases testMergesort
    where
        testQuicksort Case{..} = it explanation assertion
            where
                explanation = unwords [show input, "-", description]
                assertion   = quicksort input `shouldBe` expected

        testMergesort Case{..} = it explanation assertion
            where
                explanation = unwords [show input, "-", description]
                assertion   = mergesort input `shouldBe` expected

data Case = Case
    { description :: String
    , input       :: [ Integer ]
    , expected    :: [ Integer ]
    }

cases :: [Case]
cases =
    [ Case 
        { description = "empty"
        , input       = []
        , expected    = []
        }
    , Case 
        { description = "one element"
        , input       = [7]
        , expected    = [7]
        }
    , Case 
        { description = "two elements"
        , input       = [7, 9]
        , expected    = [7, 9]
        }
    , Case
        { description = "with negative"
        , input       = [-8,-3,-7,4,-2,6,-1,0]
        , expected    = [-8,-7,-3,-2,-1,0,4,6]
        }
    ]
