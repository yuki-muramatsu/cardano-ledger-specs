{-# LANGUAGE RecordWildCards #-}

import           Numeric.Natural

data Params = Params
    { e    :: !Double
    , c    :: !Double
    , f    :: !Double
    , r    :: !Double
    , eta  :: !Double
    , t    :: !Natural
    , tinf :: !Natural
    , tau  :: !Double
    , k    :: !Natural
    , n    :: !Natural
    } deriving Show

defaultParams :: Params
defaultParams = Params
    { e    = 0.5
    , c    = 1000
    , f    = 2000
    , r    = 0.05
    , eta  = 0.9
    , t    = 31000000000
    , tinf = 45000000000
    , tau  = 0.2
    , k    = 100
    , n    = 73
    }

rho :: Params -> Double
rho Params{..} =
    let t'    = fromIntegral t
        tinf' = fromIntegral tinf
        k'    = fromIntegral k
        n''   = recip $ fromIntegral n
    in  (t' * ((1 + r) ** n'' - 1) - (1 - tau) * f + k' * c * n'') /
        ((1 - tau) * min eta 1 * (tinf' - t'))

perYear :: Params -> Double
perYear p@Params{..} =
    let rho' = rho p
        n'   = fromIntegral n
    in  (1 + rho') ** n' - 1
