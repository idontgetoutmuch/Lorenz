import qualified Numeric.LinearAlgebra as LA
import           Numeric.GSL.ODE
import           Control.Monad
import           Data.Random

alpha = 16.0
rho = 45.92
beta = 4

dzdt :: Double -> [Double] -> [Double]
dzdt _t [x, y, z] = [ alpha * (y - x)
                      , x * (rho - z) - y
                      , x * y - beta * z
                      ]

m = odeSolve dzdt [1.0, 1.0, 1.0] (LA.vector $ take 10000 [1.0e-4, 2.0e-4..])

pop :: (Double, Double) -> IO (Double, Double)
pop p = do
  h <- exp <$> (sample $ rvar $ Normal (log (fst p)) 1.0e-1)
  l <- exp <$> (sample $ rvar $ Normal (log (snd p)) 1.0e-1)
  return (h, l)

