cmake_minimum_required(VERSION 3.1)
project(Kompilator CXX)

# Set the C++ standard to C++17.
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Locate the Bison and Flex packages.
find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)

# Define Bison and Flex targets using grammar files from the grammar directory.
BISON_TARGET(parser
    ${CMAKE_CURRENT_SOURCE_DIR}/grammar/parser.yy
    ${CMAKE_CURRENT_BINARY_DIR}/parser.tab.cc
)
FLEX_TARGET(lexer
    ${CMAKE_CURRENT_SOURCE_DIR}/grammar/lexer.l
    ${CMAKE_CURRENT_BINARY_DIR}/lexer.yy.cc
)
ADD_FLEX_BISON_DEPENDENCY(lexer parser)

# Add the compiler executable target, including source files and generated files.
add_executable(compiler
    src/main.cpp
    src/ast.cpp
    src/codegen.cpp
    src/driver.cpp
    src/mem.cpp
    src/sema.cpp
    src/safeops.cpp
    ${BISON_parser_OUTPUTS}
    ${FLEX_lexer_OUTPUTS}
)

# Specify include directories for the target.
target_include_directories(compiler PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_BINARY_DIR}
    ${FLEX_INCLUDE_DIRS}
)
