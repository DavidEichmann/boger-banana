#define CGEN_OUTPUT_INTERN
#include "Workarounds.h"
void MyWorkaroundFunctions_delete(MyWorkaroundFunctions* this_ptr)
{
    delete this_ptr;
}

MyWorkaroundFunctions* MyWorkaroundFunctions_new()
{
    return new MyWorkaroundFunctions();
}

int MyWorkaroundFunctions_getMouseRelX(OIS::Mouse* m)
{
    return MyWorkaroundFunctions::getMouseRelX(m);
}

int MyWorkaroundFunctions_getMouseRelY(OIS::Mouse* m)
{
    return MyWorkaroundFunctions::getMouseRelY(m);
}

size_t MyWorkaroundFunctions_getWindowHandler(Ogre::RenderWindow* win)
{
    return MyWorkaroundFunctions::getWindowHandler(win);
}

