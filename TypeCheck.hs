module TypeCheck
       ( thing
       ) where

import Types

import qualified Data.HashMap.Lazy as HM

-- look, here's what you do
-- look at each function
-- check that you're doing legal things inside
-- you can do that because either you're defining a thing, in which case you
-- know what type it's supposed to have, or you're using a function, in which
-- case you can check that the input and output types are right
-- the output is 'righty-o chap' or 'oi guvnah'

-- this should probably just be done during the main evaluation stage.
