enum InlineEnum { Uno, Dos = 0x2, Tres };

enum MyEnum {
    Ein,
    Zwei = 0x222,
    Drei
};

bool init(int a,
          int b) {
    return a + b;
}

int main(int argc, char* argv[]) {
    auto e1 = init(Dos + Zwei, -Tres);
    if (e1) {
        auto b1{true};
        auto b2(false);
        auto b3 = b1 &&
            b2;
        auto b4 = b3 || b1,
            b5{b2}, b6(false);
    }
    return 0;
}
