{-# LANGUAGE NamedFieldPuns #-}

-- component
import TestIO
-- package
import Discourse
import Lib
-- global
import Data.Time.Clock.POSIX
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup ""
    [ testCase "detectNewTopics" $ do
          assertEqual "if no old, select the 1 newest"
              [topic 3 3]
              (detectNewTopics [] [topic 1 1, topic 3 3, topic 2 2])
          assertEqual "if some old, select only newer"
              [topic 3 3]
              (detectNewTopics [topic 2 2] [topic 1 1, topic 3 3, topic 2 2])
          assertEqual "if time same, check ids"
              [topic 20 2, topic 22 2]
              (detectNewTopics  [topic 21 2]
                                [topic 20 2, topic 21 2, topic 22 2])
    , testCase "repostUpdates" $ do
          TestIOResult{..} <- execTestIO repostUpdates
          let effectsExpected = [ DiscourseGet "/latest.json"
                                , CacheRead
                                , CacheWrite
                                , GitterPost
                                ]
          assertEqual "effects" effectsExpected testIOResult_effects
    ]

topic :: Integer -> Integer -> Topic
topic tid time =
    Topic { topic_id = tid
          , topic_created_at = posixSecondsToUTCTime (fromIntegral time)
          }
