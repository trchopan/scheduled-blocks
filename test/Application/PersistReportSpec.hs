module Application.PersistReportSpec where
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

spec :: Spec
spec = do
  describe "persistReport" $ do
    it "persist lalal" $ do
      (1 :: Integer) `shouldBe` (1 :: Integer)
