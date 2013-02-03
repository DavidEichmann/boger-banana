module BB.Types
where

import qualified Graphics.Ogre.Types
import qualified OIS.Types

import Foreign
import Foreign.C.String
import Foreign.C.Types

type CBool = CChar -- correct?

type OIS__Mouse  = OIS.Types.Mouse
type Ogre__RenderWindow  = Graphics.Ogre.Types.RenderWindow
type Ogre__Vector3  = Graphics.Ogre.Types.Vector3

