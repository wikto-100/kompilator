#!/bin/bash
# build.sh - Automates building and cleaning for the Kompilator project.
#
# Usage:
#   ./build.sh         : Configures and builds the project.
#   ./build.sh clean   : Performs a partial clean (invokes 'make clean' in the build directory).
#   ./build.sh distclean : Removes the entire build directory.

set -e  # Exit immediately if a command fails.

BUILD_DIR="build"

usage() {
    echo "Usage: $0 [clean|distclean]"
    echo "  clean     : Run 'make clean' in the build directory (partial clean)."
    echo "  distclean : Remove the entire build directory (complete clean)."
    echo "  No arguments: Build the project."
    exit 1
}

if [ "$#" -gt 1 ]; then
    usage
fi

if [ "$#" -eq 1 ]; then
    case "$1" in
        clean)
            if [ -d "$BUILD_DIR" ]; then
                echo "Performing partial clean (make clean) in ${BUILD_DIR}..."
                cd "$BUILD_DIR"
                make clean
                echo "Partial clean completed."
            else
                echo "Build directory does not exist. Nothing to clean."
            fi
            exit 0
            ;;
        distclean)
            echo "Performing complete clean (removing the build directory)..."
            rm -rf "$BUILD_DIR"
            echo "Complete clean completed."
            exit 0
            ;;
        *)
            usage
            ;;
    esac
fi

# Default behavior: build the project.
if [ ! -d "$BUILD_DIR" ]; then
    echo "Creating build directory: $BUILD_DIR"
    mkdir -p "$BUILD_DIR"
fi

cd "$BUILD_DIR"
echo "Configuring the project with CMake..."
cmake ..

echo "Building the project..."
make

echo "Build completed successfully. You can now run the 'compiler' executable from the '${BUILD_DIR}' directory."
