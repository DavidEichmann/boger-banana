#ifndef CGEN_WORKAROUNDS_H
#define CGEN_WORKAROUNDS_H

#include <Workarounds.h>

extern "C"
{




#ifdef CGEN_HS
#endif

void MyWorkaroundFunctions_delete(MyWorkaroundFunctions* this_ptr);
MyWorkaroundFunctions* MyWorkaroundFunctions_new();
int MyWorkaroundFunctions_getMouseRelX(OIS::Mouse* m);
int MyWorkaroundFunctions_getMouseRelY(OIS::Mouse* m);
size_t MyWorkaroundFunctions_getWindowHandler(Ogre::RenderWindow* win);

}

#endif

