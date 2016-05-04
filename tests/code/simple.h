#pragma once
#include <iostream>

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
