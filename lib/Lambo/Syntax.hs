module Lambo.Syntax
  ( Expression (..)
  )
where

import Data.Text (Text)


-- | @\\f. (\\x. f (x x)) (\\x. f (x x))@
data Expression
  = Expression_Variable Text
    -- ^ @x@
  | Expression_Abstraction Text Expression
    -- ^ @\<variable\> . \<expression\>@
  | Expression_Application Expression Expression
    -- ^ @( \<expression\> \<expression\> )@
