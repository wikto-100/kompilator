#include "safeops.hpp"

namespace safeops {

bool safeAdd(long long a, long long b, long long &result) {
    // Check for addition overflow/underflow.
    if ((b > 0 && a > LLONG_MAX - b) ||
        (b < 0 && a < LLONG_MIN - b)) {
        return false;
    }
    result = a + b;
    return true;
}

bool safeSubtract(long long a, long long b, long long &result) {
    // Check for subtraction overflow/underflow.
    if ((b < 0 && a > LLONG_MAX + b) ||
        (b > 0 && a < LLONG_MIN + b)) {
        return false;
    }
    result = a - b;
    return true;
}

bool safeMultiply(long long a, long long b, long long &result) {
    // Multiplication by zero is safe.
    if (a == 0 || b == 0) {
        result = 0;
        return true;
    }
    if (a > 0) {
        if (b > 0) {
            if (a > LLONG_MAX / b)
                return false;
        } else { // b < 0
            if (b < LLONG_MIN / a)
                return false;
        }
    } else { // a < 0
        if (b > 0) {
            if (a < LLONG_MIN / b)
                return false;
        } else { // both a and b are negative
            if (-a > LLONG_MAX / -b)
                return false;
        }
    }
    result = a * b;
    return true;
}

bool safeDivide(long long a, long long b, long long &result) {
    // Prevent division by zero.
    if (b == 0)
        return false;
    // The only overflow scenario is LLONG_MIN / -1.
    if (a == LLONG_MIN && b == -1)
        return false;
    result = a / b;
    return true;
}



} // namespace safeops
