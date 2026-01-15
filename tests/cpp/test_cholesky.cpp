#define CATCH_CONFIG_MAIN
#include <catch2/catch_all.hpp>
#include <cstdio>
#include <complex>
#include <vector>
#include "Cholesky.h"

TEST_CASE("SPOCTRF - SINGLE PRECISION CHOLESKY FACTORIZATION", "[FLOAT32]") {
    int N = 4;
    float A[16] = {
        10.0f, -1.0f, 2.0f, 0.0f,
        -1.0f, 11.0f, -1.0f, 3.0f,
        2.0f, -1.0f, 10.0f, -1.0f,
        0.0f, 3.0f, -1.0f, 8.0f
    };
    float L[16] = {0};
    int LDA = N;
    int status;

    printf("\n--- TEST CASE ---\nSPOCTRF - SINGLE PRECISION CHOLESKY FACTORIZATION\n\n");

    CHOLESKY(&N, A, L, &LDA, &status);

    REQUIRE(status == 0);

    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            float sum = 0.0f;
            for (int k = 0; k <= std::min(i, j); ++k) {
                sum += L[i * N + k] * L[j * N + k];
            }
            REQUIRE_THAT(sum, Catch::Matchers::WithinAbs(A[i * N + j], 1e-5f));
        }
    }
}

TEST_CASE("DPOCTRF - DOUBLE PRECISION CHOLESKY FACTORIZATION", "[FLOAT64]") {
    int N = 4;
    double A[16] = {
        10.0, -1.0, 2.0, 0.0,
        -1.0, 11.0, -1.0, 3.0,
        2.0, -1.0, 10.0, -1.0,
        0.0, 3.0, -1.0, 8.0
    };
    double L[16] = {0};
    int LDA = N;
    int status;

    printf("\n--- TEST CASE ---\nDPOCTRF - DOUBLE PRECISION CHOLESKY FACTORIZATION\n\n");

    CHOLESKY(&N, A, L, &LDA, &status);

    REQUIRE(status == 0);

    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            double sum = 0.0;
            for (int k = 0; k <= std::min(i, j); ++k) {
                sum += L[i * N + k] * L[j * N + k];
            }
            REQUIRE_THAT(sum, Catch::Matchers::WithinAbs(A[i * N + j], 1e-10));
        }
    }
}

TEST_CASE("CPOCTRF - COMPLEX FLOAT CHOLESKY FACTORIZATION", "[COMPLEX64]") {
    int N = 4;
    std::complex<float> A[16] = {
        {10.0f, 0.0f}, {-1.0f, 0.0f}, {2.0f, 0.0f}, {0.0f, 0.0f},
        {-1.0f, 0.0f}, {11.0f, 0.0f}, {-1.0f, 0.0f}, {3.0f, 0.0f},
        {2.0f, 0.0f}, {-1.0f, 0.0f}, {10.0f, 0.0f}, {-1.0f, 0.0f},
        {0.0f, 0.0f}, {3.0f, 0.0f}, {-1.0f, 0.0f}, {8.0f, 0.0f}
    };
    std::complex<float> L[16] = {};
    int LDA = N;
    int status;

    printf("\n--- TEST CASE ---\nCPOCTRF - COMPLEX FLOAT CHOLESKY FACTORIZATION\n\n");

    CHOLESKY(&N, A, L, &LDA, &status);

    REQUIRE(status == 0);

    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            std::complex<float> sum(0, 0);
            for (int k = 0; k <= std::min(i, j); ++k) {
                sum += L[i * N + k] * std::conj(L[j * N + k]);
            }
            REQUIRE_THAT(sum.real(), Catch::Matchers::WithinAbs(A[i * N + j].real(), 1e-5f));
            REQUIRE_THAT(sum.imag(), Catch::Matchers::WithinAbs(A[i * N + j].imag(), 1e-5f));
        }
    }
}

TEST_CASE("ZPOCTRF - COMPLEX DOUBLE CHOLESKY FACTORIZATION", "[COMPLEX128]") {
    int N = 4;
    std::complex<double> A[16] = {
        {10.0, 0.0}, {-1.0, 0.0}, {2.0, 0.0}, {0.0, 0.0},
        {-1.0, 0.0}, {11.0, 0.0}, {-1.0, 0.0}, {3.0, 0.0},
        {2.0, 0.0}, {-1.0, 0.0}, {10.0, 0.0}, {-1.0, 0.0},
        {0.0, 0.0}, {3.0, 0.0}, {-1.0, 0.0}, {8.0, 0.0}
    };
    std::complex<double> L[16] = {};
    int LDA = N;
    int status;

    printf("\n--- TEST CASE ---\nZPOCTRF - COMPLEX DOUBLE CHOLESKY FACTORIZATION\n\n");

    CHOLESKY(&N, A, L, &LDA, &status);

    REQUIRE(status == 0);

    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            std::complex<double> sum(0, 0);
            for (int k = 0; k <= std::min(i, j); ++k) {
                sum += L[i * N + k] * std::conj(L[j * N + k]);
            }
            REQUIRE_THAT(sum.real(), Catch::Matchers::WithinAbs(A[i * N + j].real(), 1e-10));
            REQUIRE_THAT(sum.imag(), Catch::Matchers::WithinAbs(A[i * N + j].imag(), 1e-10));
        }
    }
}
