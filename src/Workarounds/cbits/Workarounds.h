#ifndef CGEN_WORKAROUNDS_H
#define CGEN_WORKAROUNDS_H

#include "../cpp/Workarounds.h"

extern "C"
{




#ifdef CGEN_HS
#endif

void MyWorkaroundFunctions_delete(MyWorkaroundFunctions* this_ptr);
MyWorkaroundFunctions* MyWorkaroundFunctions_new();
int MyWorkaroundFunctions_getMouseRelX(OIS::Mouse* m);
int MyWorkaroundFunctions_getMouseRelY(OIS::Mouse* m);
float MyWorkaroundFunctions_getVector3X(Ogre::Vector3* v);
float MyWorkaroundFunctions_getVector3Y(Ogre::Vector3* v);
float MyWorkaroundFunctions_getVector3Z(Ogre::Vector3* v);
size_t MyWorkaroundFunctions_getWindowHandler(Ogre::RenderWindow* win);

}

#endif

