#pragma once
#include "simple_r.h"
#include <iostream>

#ifdef MYDEF1
#define DUMMY Dummy
#else
#endif

#ifndef MYDEF1
#define DUMMY Dummy
#else
#endif

#ifndef MYDEF1
#else
typedef int integer;
#endif

#ifdef MYDEF1
#else
typedef int integer;
#endif

class Dummy {
    int i_;
    int y_;
  public:
    int stub;
    void run1() {
        std::cout << "hehe" <<  std::endl;
    }
    void run1(int a) {
        std::cout << "hehe " << a <<  std::endl;
    }
};
