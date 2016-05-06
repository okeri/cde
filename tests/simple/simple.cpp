#include <vector>
#include "simple.h"

int main(int argc, char *argv[]) {
    std::vector<Dummy> array;
    array.push_back(Dummy());
    array[0].run1(42);

#ifndef MYDEF1
array[0].r
#endif

    return 0;
}
