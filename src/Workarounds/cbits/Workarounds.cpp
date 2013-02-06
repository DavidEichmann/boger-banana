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

float MyWorkaroundFunctions_getVector3X(Ogre::Vector3* v)
{
    return MyWorkaroundFunctions::getVector3X(v);
}

float MyWorkaroundFunctions_getVector3Y(Ogre::Vector3* v)
{
    return MyWorkaroundFunctions::getVector3Y(v);
}

float MyWorkaroundFunctions_getVector3Z(Ogre::Vector3* v)
{
    return MyWorkaroundFunctions::getVector3Z(v);
}

size_t MyWorkaroundFunctions_getWindowHandler(Ogre::RenderWindow* win)
{
    return MyWorkaroundFunctions::getWindowHandler(win);
}

