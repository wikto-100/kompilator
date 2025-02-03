/**
 * \author: Wiktor Stojek nr. indeksu 272383
 */
#include <iostream>
#include <cstdlib>

#include "driver.hpp"

int main(int argc, char* argv[])
{
    // Sprawdzenie liczby argumentów
    if (argc != 3)
    {
        std::cerr << "Sposób użycia: " << argv[0] << " <plik wejściowy> <plik wyjściowy>" << std::endl;
        return EXIT_FAILURE;
    }

    Compiler::Driver driver;

    try
    {
        // Kompilacja pliku wejściowego
        driver.compile(argv[1], argv[2]);
    }
    catch (const std::exception& e)
    {
        std::cerr << "Kompilacja nieudana: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
