import numpy as np
from nulapack import crout


def test_single_precision():
    a = np.array(
        [[2.0, -1.0, -2.0], [-4.0, 6.0, 3.0], [-4.0, -2.0, 8.0]],
        dtype=np.float32,
    )

    L, U, info = crout(a)  # noqa: N806
    assert info == 0
    assert np.allclose(a, L @ U, atol=1e-5)

    # Verify U has ones on diagonal
    assert np.allclose(np.diag(U), np.ones(a.shape[0]), atol=1e-6)


def test_double_precision():
    a = np.array(
        [[2.0, -1.0, -2.0], [-4.0, 6.0, 3.0], [-4.0, -2.0, 8.0]],
        dtype=np.float64,
    )

    L, U, info = crout(a)  # noqa: N806
    assert info == 0
    assert np.allclose(a, L @ U, atol=1e-12)

    # Verify U has ones on diagonal
    assert np.allclose(np.diag(U), np.ones(a.shape[0]), atol=1e-12)


def test_complex_float():
    a = np.array(
        [[2.0, -1.0, -2.0], [-4.0, 6.0, 3.0], [-4.0, -2.0, 8.0]],
        dtype=np.complex64,
    )

    L, U, info = crout(a)  # noqa: N806
    assert info == 0
    assert np.allclose(a, L @ U, atol=1e-5)

    # Verify U has ones on diagonal
    assert np.allclose(np.diag(U), np.ones(a.shape[0], dtype=np.complex64), atol=1e-6)


def test_complex_double():
    a = np.array(
        [[2.0, -1.0, -2.0], [-4.0, 6.0, 3.0], [-4.0, -2.0, 8.0]],
        dtype=np.complex128,
    )

    L, U, info = crout(a)  # noqa: N806
    assert info == 0
    assert np.allclose(a, L @ U, atol=1e-12)

    # Verify U has ones on diagonal
    assert np.allclose(np.diag(U), np.ones(a.shape[0], dtype=np.complex128), atol=1e-12)
