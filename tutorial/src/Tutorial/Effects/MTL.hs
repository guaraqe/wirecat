module Tutorial.Effects.MTL where

import Control.Category (Category (..))
import Prelude hiding ((.))

class (Category cat) => ReadWrite cat where
  readLine :: cat () String
  writeLine :: cat String ()
