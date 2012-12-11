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
		std::cout << "rel x: " << x << std::endl;
		return x;
	};
	
	static int getMouseRelY(OIS::Mouse* m) {
		int y = m->getMouseState().Y.rel;
		std::cout << "rel y: " << y << std::endl;
		return y;
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
