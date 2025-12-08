#!/bin/bash

print_help() {
    cat <<EOF
Usage: $0 [OPTIONS]

Options:
  --build, -b     Run the build process (without tests)
  --test,  -t     Run the build process with tests
  --doc,   -d     Generate documentation
  --help,  -h     Show this help message and exit
EOF
    exit 0
}

# Default flags
BUILD=0
TEST=0
DOC=0

# No arguments = show help
if [ $# -eq 0 ]; then
    print_help
fi

# Parse arguments
while [ $# -gt 0 ]; do
    case "$1" in
        --build|-b)
            BUILD=1
            ;;
        --test|-t)
            TEST=1
            ;;
        --doc|-d)
            DOC=1
            ;;
        --help|-h)
            print_help
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help to see available options."
            exit 1
            ;;
    esac
    shift
done

# BUILD (without tests)
if [ $BUILD -eq 1 ]; then
    echo "Running build (without tests)..."
    cmake -S . -B build -DBUILD_TEST=OFF || { echo "CMake configuration failed"; exit 1; }

    cd build || { echo "Cannot change directory to build"; exit 1; }
    make || { echo "Build failed"; exit 1; }
    cd .. || { echo "Cannot change back to project root"; exit 1; }
fi

# TEST BUILD (with tests)
if [ $TEST -eq 1 ]; then
    echo "Running tests..."
    cmake -S . -B build -DBUILD_TEST=ON || { echo "CMake configuration failed"; exit 1; }

    cd build || { echo "Cannot change directory to build"; exit 1; }
    make || { echo "Build failed"; exit 1; }
    cd .. || { echo "Cannot change back to project root"; exit 1; }
fi

# DOCUMENTATION
if [ $DOC -eq 1 ]; then
    echo "Building docs..."
    doxygen Doxyfile || { echo "Doxygen failed"; exit 1; }
fi
