#pragma once

#include <ucm.h>

class UCMAppObject; // forward definition

class UCMAppDestroyer
{
  private:
    UCMAppObject* mInstance;

  public:
    ~UCMAppDestroyer();
    void initialize(UCMAppObject* app);
};

class UCMAppObject
{
  private:
    static UCMAppObject* mInstance;
    static UCMAppDestroyer destroyer;

  protected:
    UCMAppObject(){};
    UCMAppObject& operator=(UCMAppObject&);
    ~UCMAppObject() {}
    UCMAppObject(const UCMAppObject&);
    friend class UCMAppDestroyer;

  public:
    static UCMAppObject& get_instance();
}
