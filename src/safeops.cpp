/**
 * \author: Wiktor Stojek nr. indeksu 272383
 */
#include "safeops.hpp"
// Maszyna wirtualna ma nieograniczone zakresy liczb całkowitych, ale ładowane zmienne są 64-bitowe
namespace safeops
{


    bool safeAdd(long long a, long long b, long long &result)
    {
        if ((b > 0 && a > LLONG_MAX - b) ||
            (b < 0 && a < LLONG_MIN - b))
        {
            return false;
        }
        result = a + b;
        return true;
    }

    bool safeSubtract(long long a, long long b, long long &result)
    {
        if ((b < 0 && a > LLONG_MAX + b) ||
            (b > 0 && a < LLONG_MIN + b))
        {
            return false;
        }
        result = a - b;
        return true;
    }

    bool safeMultiply(long long a, long long b, long long &result)
    {
        if (a == 0 || b == 0)
        {
            result = 0;
            return true;
        }
        if (a > 0)
        {
            if (b > 0)
            {
                if (a > LLONG_MAX / b)
                    return false;
            }
            else
            { // b < 0
                if (b < LLONG_MIN / a)
                    return false;
            }
        }
        else
        { // a < 0
            if (b > 0)
            {
                if (a < LLONG_MIN / b)
                    return false;
            }
            else
            { // a < 0, b < 0
                if (-a > LLONG_MAX / -b)
                    return false;
            }
        }
        result = a * b;
        return true;
    }

    bool safeDivide(long long a, long long b, long long &result)
    {
        if (b == 0)
            return false;
        // Jedyny przypadek overflowa to LLONG_MIN / -1
        if (a == LLONG_MIN && b == -1)
            return false;
        result = a / b;
        return true;
    }

} // namespace safeops
