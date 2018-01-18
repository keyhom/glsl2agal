#ifndef _GLSLOPT_UTIL_H
#define _GLSLOPT_UTIL_H

#include <stdint.h>
#include <chrono>

extern std::chrono::steady_clock::time_point epoch;

static inline uint32_t steady_now() {
    return std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - epoch).count();
}

template <typename FN>
double steady_bench(const FN &fn) {
    auto duration = -steady_now();
    return (fn(), duration + steady_now());
}

class StopWatch {
public:

    StopWatch()
        : nTimeStart_(0)
        , nTimeStop_(0)
    {

    }

    ~StopWatch() { }

    inline void start() {
        nTimeStart_ = steady_now();
    }

    inline void stop() {
        nTimeStop_ = steady_now();
    }

    inline uint32_t getElapsedTime() const { return nTimeStop_ - nTimeStart_; }

private:

    uint32_t nTimeStart_;
    uint32_t nTimeStop_;

}; // class StopWatch

#endif // _GLSLOPT_UTIL_H
