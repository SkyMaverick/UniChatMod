#pragma once

#include <ucm.h>

class UCMObject; // forward definition

class UCMCoreDestroyer
{
  private:
    UCMObject* mInstance;

  public:
    ~UCMCoreDestroyer();
    void initialize(UCMObject* app);
};

class UCMObject
{
  private:
    static UCMObject* mInstance;
    static UCMCoreDestroyer destroyer;

  protected:
    UCMObject(){};
    UCMObject& operator=(UCMObject&);
    ~UCMObject() {}
    UCMObject(const UCMObject&);

    friend class UCMCoreDestroyer;

  public:
    static UCMObject& get_instance();
}
