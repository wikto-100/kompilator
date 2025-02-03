# Kompilator
Autor: Wiktor Stojek
Projekt kompilatora prostego języka imperatywnego generującego kod dla maszyny wirtualnej, w ramach laboratorium JFTT2024 prowadzonego w semestrze zimowym 2024/2025 przez dr. Maćka Gębalę
Technologie: Flex, Bison, C++17.
## Struktura katalogów
```
Kompilator/
├── CMakeLists.txt       # Plik konfiguracyjny CMake
├── README.md            # Ten plik
├── build/               # Katalog wynikowy kompilacji (tworzony automatycznie)
├── include/             # Pliki nagłówkowe
│   ├── ast.hpp
│   ├── codegen.hpp
│   ├── driver.hpp
│   ├── mem.hpp
│   ├── sema.hpp
│   └── scanner.hpp
├── grammar/            # Definicje gramatyk dla Flex i Bison
│   ├── lexer.l
│   └── parser.yy
└── src/                 # Pliki źródłowe
    ├── main.cpp
    ├── ast.cpp
    ├── codegen.cpp
    ├── driver.cpp
    ├── mem.cpp
    └── sema.cpp
```

## Wymagania

Upewnić się, że przed kompilacją projektu są zainstalowane:

- **CMake** (wersja 3.1 lub nowsza)
- Kompilator **C++** z obsługą standardu C++17 (np. GCC 14.2.1)
- **Flex** (2.6.4)
- **Bison** (3.8.2)

## Budowanie projektu

Istnieją dwa podstawowe sposoby budowania projektu: ręczny lub za pomocą dołączonego skryptu automatyzującego budowanie.

### Ręczny proces budowania

1. **Utworzenie i przejście do katalogu build:**

   ```bash
   mkdir -p build
   cd build
   ```

2. **Konfiguracja za pomocą CMake:**

   ```bash
   cmake ..
   ```

3. **Kompilacja:**

   ```bash
   make
   ```

Wynikowy plik wykonywalny (`compiler`) znajdzie się w katalogu `build/`.

### Automatyczny skrypt budujący

W katalogu głównym projektu znajduje się skrypt `build.sh`, który automatyzuje proces budowania oraz czyszczenia.
Aby z niego skorzystać:

1. **Upewnić się, że skrypt ma prawa wykonywania:**

   ```bash
   chmod +x build.sh
   ```

2. **Uruchomić skrypt budujący:**

   ```bash
   ./build.sh
   ```

## Czyszczenie budowy

Czyszczenie można przeprowadzić na dwa sposoby:

- **Czyszczenie częściowe:**  
  Usuwa artefakty wygenerowane podczas procesu budowania bez usuwania plików konfiguracyjnych CMake. Można ręcznie wykonać polecenie `make clean` w katalogu build:

  ```bash
  cd build
  make clean
  ```

  lub użyć skryptu budującego:

  ```bash
  ./build.sh clean
  ```

- **Czyszczenie całkowite (Distclean):**  
  Usuwa cały katalog build, umożliwiając rozpoczęcie budowy od początku:

  ```bash
  ./build.sh distclean
  ```

## Uruchamianie kompilatora

Po pomyślnym zbudowaniu projektu, uruchomić kompilator z katalogu build:

```bash
./compiler \<plik wejściowy\> \<plik wyjściowy\>
```

## Licencja

Projekt przeznaczony jest wyłącznie do celów akademickich. Zawiera on elementy wygenerowane przez Bison, który jest dystrybuowany na licencji GNU General Public License (GPL) z wyjątkowym pozwoleniem na użycie szkieletu parsera w oprogramowaniu nieobjętym GPL, oraz elementy wygenerowane przez Flex, który jest dystrybuowany na licencji BSD (styl BSD). Źródła tego projektu są dostępne na zasadach licencyjnych przeznaczonych do użytku akademickiego.
