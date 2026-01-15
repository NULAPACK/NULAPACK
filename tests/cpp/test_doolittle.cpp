#define CATCH_CONFIG_MAIN
#include <catch2/catch_all.hpp>
#include <cstdio>
#include <complex>
#include <vector>
#include "Doolittle.h"

TEST_CASE("SGEDTRF - SINGLE PRECISION DOOLITTLE DECOMPOSITION", "[FLOAT32]") {
    int N = 3;
    float A[9] = {
        2.0f, -1.0f, -2.0f,
        -4.0f, 6.0f, 3.0f,
        -4.0f, -2.0f, 8.0f
    };
    float L[9] = {0};
    float U[9] = {0};
    int status;

    printf("\n--- TEST CASE ---\nSGEDTRF - SINGLE PRECISION DOOLITTLE DECOMPOSITION\n\n");

    DOOLITTLE(&N, A, L, U, &status);

    REQUIRE(status == 0);

    // Verify L * U = A
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            float sum = 0.0f;
            for (int k = 0; k < N; ++k) {
                sum += L[i * N + k] * U[k * N + j];
            }
            REQUIRE_THAT(sum, Catch::Matchers::WithinAbs(A[i * N + j], 1e-5f));
        }
    }
}

TEST_CASE("DGEDTRF - DOUBLE PRECISION DOOLITTLE DECOMPOSITION", "[FLOAT64]") {
    int N = 3;
    double A[9] = {
        2.0, -1.0, -2.0,
        -4.0, 6.0, 3.0,
        -4.0, -2.0, 8.0
    };
    double L[9] = {0};
    double U[9] = {0};
    int status;

    printf("\n--- TEST CASE ---\nDGEDTRF - DOUBLE PRECISION DOOLITTLE DECOMPOSITION\n\n");

    DOOLITTLE(&N, A, L, U, &status);

    REQUIRE(status == 0);

    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            double sum = 0.0;
            for (int k = 0; k < N; ++k) {
                sum += L[i * N + k] * U[k * N + j];
            }
            REQUIRE_THAT(sum, Catch::Matchers::WithinAbs(A[i * N + j], 1e-10));
        }
    }
}

TEST_CASE("CGEDTRF - COMPLEX FLOAT DOOLITTLE DECOMPOSITION", "[COMPLEX64]") {
    int N = 3;
    std::complex<float> A[9] = {
        {2.0f, 0.0f}, {-1.0f, 0.0f}, {-2.0f, 0.0f},
        {-4.0f, 0.0f}, {6.0f, 0.0f}, {3.0f, 0.0f},
        {-4.0f, 0.0f}, {-2.0f, 0.0f}, {8.0f, 0.0f}
    };
    std::complex<float> L[9] = {};
    std::complex<float> U[9] = {};
    int status;

    printf("\n--- TEST CASE ---\nCGEDTRF - COMPLEX FLOAT DOOLITTLE DECOMPOSITION\n\n");

    DOOLITTLE(&N, A, L, U, &status);

    REQUIRE(status == 0);

    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            std::complex<float> sum(0, 0);
            for (int k = 0; k < N; ++k) {
                sum += L[i * N + k] * U[k * N + j];
            }
            REQUIRE_THAT(sum.real(), Catch::Matchers::WithinAbs(A[i * N + j].real(), 1e-5f));
            REQUIRE_THAT(sum.imag(), Catch::Matchers::WithinAbs(A[i * N + j].imag(), 1e-5f));
        }
    }
}

TEST_CASE("ZGEDTRF - COMPLEX DOUBLE DOOLITTLE DECOMPOSITION", "[COMPLEX128]") {
    int N = 3;
    std::complex<double> A[9] = {
        {2.0, 0.0}, {-1.0, 0.0}, {-2.0, 0.0},
        {-4.0, 0.0}, {6.0, 0.0}, {3.0, 0.0},
        {-4.0, 0.0}, {-2.0, 0.0}, {8.0, 0.0}
    };
    std::complex<double> L[9] = {};
    std::complex<double> U[9] = {};
    int status;

    printf("\n--- TEST CASE ---\nZGEDTRF - COMPLEX DOUBLE DOOLITTLE DECOMPOSITION\n\n");

    DOOLITTLE(&N, A, L, U, &status);

    REQUIRE(status == 0);

    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            std::complex<double> sum(0, 0);
            for (int k = 0; k < N; ++k) {
                sum += L[i * N + k] * U[k * N + j];
            }
            REQUIRE_THAT(sum.real(), Catch::Matchers::WithinAbs(A[i * N + j].real(), 1e-10));
            REQUIRE_THAT(sum.imag(), Catch::Matchers::WithinAbs(A[i * N + j].imag(), 1e-10));
        }
    }
}
