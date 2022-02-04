module Constants where

dimension = 3 :: Int

inf = 999999 :: Double



newtype MyColor4 = MyColor4 (Float, Float, Float, Float)
    deriving (Eq, Ord)

whiteColor = MyColor4 (1, 1, 1, 1)
purpleColor = MyColor4 (1, 0, 1, 1)
blackColor = MyColor4 (0, 0, 0, 1)
redColor = MyColor4 (1, 0, 0, 1)
greenColor = MyColor4 (0, 1, 0, 1)
blueColor = MyColor4 (0, 0, 1, 1)
cyanColor = MyColor4 (0, 1, 1, 1)
brownColor = MyColor4 (1, 1, 0, 1)