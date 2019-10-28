#include <ucm.h>

UCMAppDestroyer::~UCMAppDestroyer() { delete mInstance; }

void
UCMAppDestroyer::initialize(UCMAppObject* app) {
    mInstance = app;
}

UCMAppObject&
UCMAppObject::get_instance() {
    if (!mInstance) {
        mInstance = new UCMAppObject();
        destroyer.initialize(mInstance);
    }
    return *mInstance;
}
