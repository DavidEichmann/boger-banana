module BB.Types
where

import Foreign
import Foreign.C.String
import Foreign.C.Types

type CBool = CChar -- correct?

newtype OIS__Mouse = OIS__Mouse (Ptr OIS__Mouse) -- nullary data type
newtype Ogre__RenderWindow = Ogre__RenderWindow (Ptr Ogre__RenderWindow) -- nullary data type
newtype Ogre__Vector3 = Ogre__Vector3 (Ptr Ogre__Vector3) -- nullary data type

