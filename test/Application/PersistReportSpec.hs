module Application.PersistReportSpec where

import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

spec :: Spec
spec = do
  describe "persistReport" $ do
    it "persist pass" $ do
      (2 :: Integer) `shouldBe` (1 :: Integer)
