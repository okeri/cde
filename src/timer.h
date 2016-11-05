#pragma once

#include <chrono>
#include <string>
#include <fstream>

class Timer {
    std::chrono::time_point<std::chrono::high_resolution_clock> time_;

  public:
    void dump_start(const std::string &msg) {
        time_ = std::chrono::high_resolution_clock::now();
        std::ofstream out("/tmp/cdedebug.txt", std::ios::app);
        std::time_t now = std::chrono::system_clock::to_time_t(time_);
        out << "[" << std::ctime(&now) << "] " << msg << std::endl;
    }

    void dump_end(const std::string &msg) {
        auto mks = std::chrono::duration_cast<std::chrono::microseconds>(
            std::chrono::high_resolution_clock::now() - time_).count();
        std::ofstream out("/tmp/cdedebug.txt", std::ios::app);
        out << "[ dur: " << mks << " mks] " << msg << std::endl;
    }
};
