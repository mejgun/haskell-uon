module UON
  ( UON.Internal.Request.newKey,
    UON.Internal.Request.Key,
    pause,
  )
where

import Control.Concurrent (threadDelay)
import UON.Internal.Request qualified

-- 1 секундная пауза (для запросов)
pause :: IO ()
pause = threadDelay 1000000