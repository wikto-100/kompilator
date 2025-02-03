#ifndef SAFEOPS_HPP
#define SAFEOPS_HPP

#include <climits> // For LLONG_MAX and LLONG_MIN

namespace safeops {

/**
 * @brief Safely adds two long long integers.
 *
 * @param a      The first operand.
 * @param b      The second operand.
 * @param result Output parameter where the sum is stored if successful.
 * @return true  If the addition does not overflow.
 * @return false Otherwise.
 */
bool safeAdd(long long a, long long b, long long &result);

/**
 * @brief Safely subtracts two long long integers.
 *
 * @param a      The minuend.
 * @param b      The subtrahend.
 * @param result Output parameter where the difference is stored if successful.
 * @return true  If the subtraction does not overflow.
 * @return false Otherwise.
 */
bool safeSubtract(long long a, long long b, long long &result);

/**
 * @brief Safely multiplies two long long integers.
 *
 * @param a      The first operand.
 * @param b      The second operand.
 * @param result Output parameter where the product is stored if successful.
 * @return true  If the multiplication does not overflow.
 * @return false Otherwise.
 */
bool safeMultiply(long long a, long long b, long long &result);

/**
 * @brief Safely divides two long long integers.
 *
 * @param a      The numerator.
 * @param b      The denominator.
 * @param result Output parameter where the quotient is stored if successful.
 * @return true  If the division is valid and does not overflow.
 * @return false Otherwise (e.g., division by zero or overflow case).
 */
bool safeDivide(long long a, long long b, long long &result);

/**
 * @brief Safely computes the modulo of two long long integers.
 *
 * @param a      The dividend.
 * @param b      The divisor.
 * @param result Output parameter where the modulo result is stored if successful.
 * @return true  If the operation is valid.
 * @return false Otherwise (e.g., modulo by zero).
 */
bool safeModulo(long long a, long long b, long long &result);

} // namespace safeops

#endif // SAFEOPS_HPP
