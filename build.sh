#!/bin/bash
# Autor: Wiktor Stojek, nr indeksu: 272383
# build.sh - Automatyzuje budowanie i czyszczenie projektu Kompilator.
#
# Użycie:
#   ./build.sh         : Konfiguruje i buduje projekt.
#   ./build.sh clean   : Wykonuje częściowe czyszczenie (wywołuje 'make clean' w katalogu build).
#   ./build.sh distclean : Usuwa cały katalog build.

set -e  # Zakończ natychmiast, jeśli polecenie nie powiedzie się.

BUILD_DIR="build"

usage() {
    echo "Użycie: $0 [clean|distclean]"
    echo "  clean     : Uruchom 'make clean' w katalogu build (częściowe czyszczenie)."
    echo "  distclean : Usuń cały katalog build (pełne czyszczenie)."
    echo "  Brak argumentów: Zbuduj projekt."
    exit 1
}

if [ "$#" -gt 1 ]; then
    usage
fi

if [ "$#" -eq 1 ]; then
    case "$1" in
        clean)
            if [ -d "$BUILD_DIR" ]; then
                echo "Wykonywanie częściowego czyszczenia (make clean) w ${BUILD_DIR}..."
                cd "$BUILD_DIR"
                make clean
                echo "Częściowe czyszczenie zakończone."
            else
                echo "Katalog build nie istnieje. Nie ma nic do wyczyszczenia."
            fi
            exit 0
            ;;
        distclean)
            echo "Wykonywanie pełnego czyszczenia (usuwanie katalogu build)..."
            rm -rf "$BUILD_DIR"
            echo "Pełne czyszczenie zakończone."
            exit 0
            ;;
        *)
            usage
            ;;
    esac
fi

# Domyślne zachowanie: zbuduj projekt.
if [ ! -d "$BUILD_DIR" ]; then
    echo "Tworzenie katalogu build: $BUILD_DIR"
    mkdir -p "$BUILD_DIR"
fi

cd "$BUILD_DIR"
echo "Konfigurowanie projektu za pomocą CMake..."
cmake ..

echo "Budowanie projektu..."
make

echo "Budowanie zakończone pomyślnie. Można teraz uruchomić plik wykonywalny 'kompilator' z katalogu '${BUILD_DIR}'."
