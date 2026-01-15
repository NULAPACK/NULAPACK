#define CATCH_CONFIG_MAIN
#include <catch2/catch_all.hpp>
#include <cstdio>
#include <complex>
#include "Thomas.h"

TEST_CASE("SGTTSV - SINGLE PRECISION THOMAS SOLVER", "[FLOAT32]") {
    int N = 4;
    float A[16] = {
        10.0f, -1.0f, 0.0f, 0.0f,
        -1.0f, 11.0f, -1.0f, 0.0f,
        0.0f, -1.0f, 10.0f, -1.0f,
        0.0f, 0.0f, -1.0f, 8.0f
    };
    float B[4] = {6.0f, 25.0f, -11.0f, 15.0f};
    float X[4] = {};
    int status;

    printf("\n--- TEST CASE ---\nSGTTSV - SINGLE PRECISION THOMAS SOLVER\n\n");

    THOMAS(&N, A, B, X, &status);

    for (int i = 0; i < N; i++) printf("X[%d] = %.6f\n", i, X[i]);

    REQUIRE(status == 0);
    REQUIRE_THAT(X[0], Catch::Matchers::WithinAbs(372.0f / 449.0f, 1e-5f));
    REQUIRE_THAT(X[1], Catch::Matchers::WithinAbs(1026.0f / 449.0f, 1e-5f));
    REQUIRE_THAT(X[2], Catch::Matchers::WithinAbs(-311.0f / 449.0f, 1e-5f));
    REQUIRE_THAT(X[3], Catch::Matchers::WithinAbs(803.0f / 449.0f, 1e-5f));
}

TEST_CASE("DGTTSV - DOUBLE PRECISION THOMAS SOLVER", "[FLOAT64]") {
    int N = 4;
    double A[16] = {
        10.0, -1.0, 0.0, 0.0,
        -1.0, 11.0, -1.0, 0.0,
        0.0, -1.0, 10.0, -1.0,
        0.0, 0.0, -1.0, 8.0
    };
    double B[4] = {6.0, 25.0, -11.0, 15.0};
    double X[4] = {};
    int status;

    printf("\n--- TEST CASE ---\nDGTTSV - DOUBLE PRECISION THOMAS SOLVER\n\n");

    THOMAS(&N, A, B, X, &status);

    for (int i = 0; i < N; i++) printf("X[%d] = %.12lf\n", i, X[i]);

    REQUIRE(status == 0);
    REQUIRE_THAT(X[0], Catch::Matchers::WithinAbs(372.0 / 449.0, 1e-12));
    REQUIRE_THAT(X[1], Catch::Matchers::WithinAbs(1026.0 / 449.0, 1e-12));
    REQUIRE_THAT(X[2], Catch::Matchers::WithinAbs(-311.0 / 449.0, 1e-12));
    REQUIRE_THAT(X[3], Catch::Matchers::WithinAbs(803.0 / 449.0, 1e-12));
}

TEST_CASE("CGTTSV - COMPLEX FLOAT THOMAS SOLVER", "[COMPLEX64]") {
    using cf = std::complex<float>;
    int N = 4;
    cf A[16] = {
        {10.0f, 0.0f}, {-1.0f, 0.0f}, {0.0f, 0.0f}, {0.0f, 0.0f},
        {-1.0f, 0.0f}, {11.0f, 0.0f}, {-1.0f, 0.0f}, {0.0f, 0.0f},
        {0.0f, 0.0f}, {-1.0f, 0.0f}, {10.0f, 0.0f}, {-1.0f, 0.0f},
        {0.0f, 0.0f}, {0.0f, 0.0f}, {-1.0f, 0.0f}, {8.0f, 0.0f}
    };
    cf B[4] = { {6.0f, 0.0f}, {25.0f, 0.0f}, {-11.0f, 0.0f}, {15.0f, 0.0f} };
    cf X[4] = {};
    int status;

    printf("\n--- TEST CASE ---\nCGTTSV - COMPLEX FLOAT THOMAS SOLVER\n\n");

    THOMAS(&N, A, B, X, &status);

    for (int i = 0; i < N; i++) {
        printf("X[%d] = (%.6f, %.6f)\n", i, X[i].real(), X[i].imag());
    }

    REQUIRE(status == 0);
    REQUIRE_THAT(X[0].real(), Catch::Matchers::WithinAbs(372.0f / 449.0f, 1e-5f));
    REQUIRE_THAT(X[0].imag(), Catch::Matchers::WithinAbs(0.0f, 1e-5f));
    REQUIRE_THAT(X[1].real(), Catch::Matchers::WithinAbs(1026.0f / 449.0f, 1e-5f));
    REQUIRE_THAT(X[1].imag(), Catch::Matchers::WithinAbs(0.0f, 1e-5f));
    REQUIRE_THAT(X[2].real(), Catch::Matchers::WithinAbs(-311.0f / 449.0f, 1e-5f));
    REQUIRE_THAT(X[2].imag(), Catch::Matchers::WithinAbs(0.0f, 1e-5f));
    REQUIRE_THAT(X[3].real(), Catch::Matchers::WithinAbs(803.0f / 449.0f, 1e-5f));
    REQUIRE_THAT(X[3].imag(), Catch::Matchers::WithinAbs(0.0f, 1e-5f));
}

TEST_CASE("ZGTTSV - COMPLEX DOUBLE THOMAS SOLVER", "[COMPLEX128]") {
    using cd = std::complex<double>;
    int N = 4;
    cd A[16] = {
        {10.0, 0.0}, {-1.0, 0.0}, {0.0, 0.0}, {0.0, 0.0},
        {-1.0, 0.0}, {11.0, 0.0}, {-1.0, 0.0}, {0.0, 0.0},
        {0.0, 0.0}, {-1.0, 0.0}, {10.0, 0.0}, {-1.0, 0.0},
        {0.0, 0.0}, {0.0, 0.0}, {-1.0, 0.0}, {8.0, 0.0}
    };
    cd B[4] = { {6.0, 0.0}, {25.0, 0.0}, {-11.0, 0.0}, {15.0, 0.0} };
    cd X[4] = {};
    int status;

    printf("\n--- TEST CASE ---\nZGTTSV - COMPLEX DOUBLE THOMAS SOLVER\n\n");

    THOMAS(&N, A, B, X, &status);

    for (int i = 0; i < N; i++) {
        printf("X[%d] = (%.12lf, %.12lf)\n", i, X[i].real(), X[i].imag());
    }

    REQUIRE(status == 0);
    REQUIRE_THAT(X[0].real(), Catch::Matchers::WithinAbs(372.0 / 449.0, 1e-12));
    REQUIRE_THAT(X[0].imag(), Catch::Matchers::WithinAbs(0.0, 1e-12));
    REQUIRE_THAT(X[1].real(), Catch::Matchers::WithinAbs(1026.0 / 449.0, 1e-12));
    REQUIRE_THAT(X[1].imag(), Catch::Matchers::WithinAbs(0.0, 1e-12));
    REQUIRE_THAT(X[2].real(), Catch::Matchers::WithinAbs(-311.0 / 449.0, 1e-12));
    REQUIRE_THAT(X[2].imag(), Catch::Matchers::WithinAbs(0.0, 1e-12));
    REQUIRE_THAT(X[3].real(), Catch::Matchers::WithinAbs(803.0 / 449.0, 1e-12));
    REQUIRE_THAT(X[3].imag(), Catch::Matchers::WithinAbs(0.0, 1e-12));
}
