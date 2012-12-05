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
		return m->getMouseState().X.rel;
	};
	
	static int getMouseRelY(OIS::Mouse* m) {
		return m->getMouseState().Y.rel;
	};
	
	
	//
	// OGRE
	//
	
	static size_t getWindowHandler(Ogre::RenderWindow* win) {
		OIS::ParamList pl;
		size_t windowHnd = 0;
		std::ostringstream windowHndStr;
		win->getCustomAttribute("WINDOW", &windowHnd);
		return windowHnd;
	};
};
