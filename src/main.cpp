#include <iostream>
#include <cstdlib>

#include "driver.hpp"

int main(int argc, char* argv[])
{
    // Verify that exactly two command line arguments (excluding the program name) are provided.
    if (argc != 3)
    {
        std::cerr << "Usage: " << argv[0] << " <source_file> <output_file>" << std::endl;
        return EXIT_FAILURE;
    }

    Compiler::Driver driver;

    try
    {
        // Compile the source file to the specified output file.
        driver.compile(argv[1], argv[2]);
    }
    catch (const std::exception& e)
    {
        std::cerr << "Compilation failed: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
