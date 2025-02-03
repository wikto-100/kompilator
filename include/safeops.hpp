
#ifndef SAFEOPS_HPP
#define SAFEOPS_HPP
/**
 * \author: Wiktor Stojek nr. indeksu 272383
 */
#include <climits> // Do LLONG_MAX i LLONG_MIN

namespace safeops {

/**
 * @brief Bezpiecznie dodaje dwie liczby całkowite typu long long.
 *
 * @param a      Pierwszy argument
 * @param b      Drugi argument
 * @param result Parametr wyjściowy, gdzie zostanie zapisana suma jeśli operacja się powiedzie.
 * @return true  Jeśli dodawanie nie powoduje przepełnienia.
 * @return false W przeciwnym razie.
 */
bool safeAdd(long long a, long long b, long long &result);

/**
 * @brief Bezpiecznie odejmuje dwie liczby całkowite typu long long.
 *
 * @param a      Odjemna.
 * @param b      Odjemnik.
 * @param result Parametr wyjściowy, gdzie zostanie zapisana różnica jeśli operacja się powiedzie.
 * @return true  Jeśli odejmowanie nie powoduje przepełnienia.
 * @return false W przeciwnym razie.
 */
bool safeSubtract(long long a, long long b, long long &result);

/**
 * @brief Bezpiecznie mnoży dwie liczby całkowite typu long long.
 *
 * @param a      Pierwszy argument.
 * @param b      Drugi argument.
 * @param result Parametr wyjściowy, gdzie zostanie zapisany iloczyn jeśli operacja się powiedzie.
 * @return true  Jeśli mnożenie nie powoduje przepełnienia.
 * @return false W przeciwnym razie.
 */
bool safeMultiply(long long a, long long b, long long &result);

/**
 * @brief Bezpiecznie dzieli dwie liczby całkowite typu long long.
 *
 * @param a      Licznik.
 * @param b      Mianownik.
 * @param result Parametr wyjściowy, gdzie zostanie zapisany iloraz jeśli operacja się powiedzie.
 * @return true  Jeśli dzielenie jest poprawne i nie powoduje przepełnienia.
 * @return false W przeciwnym razie (np. dzielenie przez zero lub przepełnienie).
 */
bool safeDivide(long long a, long long b, long long &result);

/**
 * @brief Bezpiecznie oblicza modulo z dwóch liczb całkowitych typu long long.
 *
 * @param a      Dzielna.
 * @param b      Dzielnik.
 * @param result Parametr wyjściowy, gdzie zostanie zapisany wynik modulo jeśli operacja się powiedzie.
 * @return true  Jeśli operacja jest poprawna.
 * @return false W przeciwnym razie (np. modulo przez zero).
 */
bool safeModulo(long long a, long long b, long long &result);

} // namespace safeops

#endif // SAFEOPS_HPP
