module Cointoss
  () where

-- import           System.Random                  ( Random(random)
--                                                 , StdGen
--                                                 )

import           System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin , gen' ) = random gen
      (secondCoin, gen'') = random gen'
      (thirdCoin , _    ) = random gen''
  in  (firstCoin, secondCoin, thirdCoin)





