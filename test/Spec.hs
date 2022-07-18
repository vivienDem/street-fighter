import Test.Hspec
import ModelSpec as MS

main :: IO ()
main = hspec $ do
    MS.modelSpec
