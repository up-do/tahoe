module Tahoe.Capability.Internal.Confidential where

import qualified Data.Text as T

{- | Something which contains confidential information and can be rendered as
 text such that the text also includes confidential information.  It is
 expected (but not required) that such types will also have a Show instance
 which obscures the confidential information.
-}
class ConfidentialShowable s where
    -- | Show the value, including any confidential information.
    confidentiallyShow :: s -> T.Text
