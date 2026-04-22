module Tutorial.Effects.Mixed where

import Control.Category (Category (..), (>>>))
import Prelude hiding ((.))

data ReadWrite a b where
  ReadLine :: ReadWrite () String
  WriteLine :: ReadWrite String ()

class (Category cat) => Interpret cat eff where
  interpret :: eff a b -> cat a b

type eff :> cat = Interpret cat eff

pipeline :: (ReadWrite :> cat) => cat () ()
pipeline =
  interpret ReadLine >>> interpret WriteLine

data ReadWriteM a where
  ReadLineM :: ReadWriteM String
  WriteLineM :: String -> ReadWriteM ()

class (Monad m) => InterpretM m eff where
  interpretM :: eff a -> m a

pipelineM :: (InterpretM m ReadWriteM) => m ()
pipelineM = do
  line <- interpretM ReadLineM
  interpretM (WriteLineM line)
