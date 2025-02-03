# Autor: Wiktor Stojek, nr. indeksu 272383
cmake_minimum_required(VERSION 3.1)
project(Kompilator CXX)

# Ustaw standard C++ na C++17.
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Znajdź programy Bison i Flex.
find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)

# Wygeneruj pliki źródłowe z plików Bison i Flex.
BISON_TARGET(parser
    ${CMAKE_CURRENT_SOURCE_DIR}/grammar/parser.yy
    ${CMAKE_CURRENT_BINARY_DIR}/parser.tab.cc
)
FLEX_TARGET(lexer
    ${CMAKE_CURRENT_SOURCE_DIR}/grammar/lexer.l
    ${CMAKE_CURRENT_BINARY_DIR}/lexer.yy.cc
)
ADD_FLEX_BISON_DEPENDENCY(lexer parser)

# Dodaj pliki źródłowe do docelowego programu kompilatora.
add_executable(kompilator
    src/main.cpp
    src/codegen.cpp
    src/driver.cpp
    src/mem.cpp
    src/sema.cpp
    src/safeops.cpp
    ${BISON_parser_OUTPUTS}
    ${FLEX_lexer_OUTPUTS}
)

# Dodaj katalogi z plikami nagłówkowymi do kompilacji.
target_include_directories(kompilator PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_BINARY_DIR}
    ${FLEX_INCLUDE_DIRS}
)
