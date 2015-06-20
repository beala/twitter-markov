import qualified TwitterMarkov.Tests.MarkovModel as MarkovModel

import Test.Tasty

main :: IO ()
main = defaultMain MarkovModel.tests

