#include <iostream>
#include "OIS/OISMouse.h"
#include "OIS/OISPrereqs.h"
#include "OGRE/OgreRenderWindow.h"

class MyWorkaroundFunctions {
public:

	//
	// OIS
	//
	
	static int getMouseRelX(OIS::Mouse* m) {
		int x = m->getMouseState().X.rel;
		return x;
	};
	
	static int getMouseRelY(OIS::Mouse* m) {
		int y = m->getMouseState().Y.rel;
		return y;
	};
	
	
	//
	// OGRE
	//
	
	static float getVector3X(Ogre::Vector3* v) {
		return v->x;
	}
	
	static float getVector3Y(Ogre::Vector3* v) {
		return v->y;
	}
	
	static float getVector3Z(Ogre::Vector3* v) {
		return v->z;
	}
	
	static size_t getWindowHandler(Ogre::RenderWindow* win) {
		size_t windowHnd = 0;
		win->getCustomAttribute("WINDOW", &windowHnd);
		return windowHnd;
	};
};
