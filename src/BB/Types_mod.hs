module BB.Types
where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Graphics.Ogre.Types
import OIS.Types

type CBool = CChar -- correct?

{-
        Modified from cgen-hs output to use already available Graphics.Ogre.Types and OIS.Types
-}
type OIS__Mouse = Mouse-- nullary data type
type Ogre__RenderWindow = RenderWindow -- nullary data type

