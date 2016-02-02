-- TODO: Revise and PR to purescript-dom
module DOM.XHR.FormData
  ( empty
  , insert
  ) where

import Prelude (Unit, unit)
import Control.Monad.Eff (Eff(), Pure())
import Data.StrMap (StrMap)
import Data.StrMap as StrMap

import DOM.XHR.Types (FormData)

-- | Create an empty `FormData` object
foreign import empty :: Pure FormData

-- | Inserts a new value onto an existing key inside a `FormData`, or adds
--   the key if it does not already exist. 
foreign import insert :: forall a. String -> a -> FormData -> Pure FormData