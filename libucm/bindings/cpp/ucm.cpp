#include <ucm.h>

UCMAppDestroyer::~UCMAppDestroyer()
{
    delete mInstance;
}

void
UCMAppDestroyer::initialize(UCMAppAbstract* app)
{
    mInstance = app;
}

UCMAppAbstract&
UCMAppAbstract::get_instance()
{
    if (!mInstance) {
        mInstance = new UCMAppAbstract();
        destroyer.initialize(mInstance);
    }
    return *mInstance;
}
