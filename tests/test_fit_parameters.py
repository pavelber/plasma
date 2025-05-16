import os
import sys

# Add project root to sys.path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

import pytest
import numpy as np
from lib.fit_parameters import create_fits, create_fits_from_range, select_best_fit


# Existing fixtures
@pytest.fixture
def setup_params():
    return {
        'start_e': 1.0,
        'end_e': 10.0,
        'step_e': 1.0,
        'ionization_potential': 1.0,
        'stat_weight': 2.0,
        'bad_fit_threshold': 1e-16
    }


# ... existing tests (test_successful_fit_default_approximation, test_quadratic_fit, etc.)

def test_create_fits_table():
    """
    Test create_fits with a table of 10 points from y = 3 + 5x + 4x^2.
    Uses approximation function a + bx + cx^2, expecting parameters [3.0, 5.0, 4.0].
    """
    # Generate table
    x_values = np.linspace(1.0, 10.0, 10)  # 10 points
    y_values = 3.0 + 5.0 * x_values + 4.0 * x_values ** 2
    table = list(zip(x_values, y_values))

    # Approximation function
    def approximation_fun(params, E0, stat_weight, x):
        a, b, c = params
        return a + b * x + c * x ** 2

    # Initial guess
    initial_params = [2.0, 4.0, 3.0]

    # Call create_fits
    (result, square_diff) = create_fits(
        table=table,
        approximation_fun=approximation_fun,
        ionization_potential=1.0,
        stat_weight=2.0,
        initial_params=initial_params,
        method='powell',
        reject_bad_fits=False,
        bad_fit_threshold=1e-16
    )

    # Debugging
    print(f"Table fit parameters: {result}")

    # Assertions
    assert result is not None, "Fit should succeed"
    assert len(result) == 3, "Should return 3 parameters"
    assert all(np.isfinite(result)), "All parameters should be finite"
    assert np.isclose(result[0], 3.0, atol=1e-2), f"Parameter a should be 3.0, got {result[0]}"
    assert np.isclose(result[1], 5.0, atol=1e-2), f"Parameter b should be 5.0, got {result[1]}"
    assert np.isclose(result[2], 4.0, atol=1e-2), f"Parameter c should be 4.0, got {result[2]}"

    # Verify fit quality
    def fun_to_minimize(params):
        s = 0
        for x, y in table:
            v = approximation_fun(params, 1.0, 2.0, x)
            s += (v - y) * (v - y)
        return s

    sum_square_diffs = fun_to_minimize(result)
    print(f"Table fit sum of squared differences: {sum_square_diffs}")
    assert sum_square_diffs < 1e-10, f"Fit error too large: {sum_square_diffs}"


def test_create_fits_from_range(setup_params):
    """
    Test create_fits_from_range with an energy function y = 3 + 5x + 4x^2.
    Generates a table over a range and fits a + bx + cx^2, expecting [3.0, 5.0, 4.0].
    """

    # Define energy function
    def energy_function(x):
        return 3.0 + 5.0 * x + 4.0 * x ** 2

    # Approximation function
    def approximation_fun(params, E0, stat_weight, x):
        a, b, c = params
        return a + b * x + c * x ** 2

    # Initial guess
    initial_params = [2.0, 4.0, 3.0]

    # Call create_fits_from_range
    (result, square_diff) = create_fits_from_range(
        cross_cut_function=energy_function,
        approximation_fun=approximation_fun,
        ionization_potential=setup_params['ionization_potential'],
        stat_weight=setup_params['stat_weight'],
        initial_params=initial_params,
        start_e=setup_params['start_e'],
        end_e=setup_params['end_e'],
        step_e=setup_params['step_e'],
        method='powell',
        reject_bad_fits=False,
        bad_fit_threshold=setup_params['bad_fit_threshold']
    )

    # Debugging
    print(f"Range fit parameters: {result}")

    # Assertions
    assert result is not None, "Fit should succeed"
    assert len(result) == 3, "Should return 3 parameters"
    assert all(np.isfinite(result)), "All parameters should be finite"
    assert np.isclose(result[0], 3.0, atol=1e-2), f"Parameter a should be 3.0, got {result[0]}"
    assert np.isclose(result[1], 5.0, atol=1e-2), f"Parameter b should be 5.0, got {result[1]}"
    assert np.isclose(result[2], 4.0, atol=1e-2), f"Parameter c should be 4.0, got {result[2]}"

    # Verify fit quality
    x_values = np.arange(setup_params['start_e'], setup_params['end_e'] + setup_params['step_e'],
                         setup_params['step_e'])
    y_values = energy_function(x_values)

    def fun_to_minimize(params):
        s = 0
        for x, y in zip(x_values, y_values):
            v = approximation_fun(params, setup_params['ionization_potential'], setup_params['stat_weight'], x)
            s += (v - y) * (v - y)
        return s

    sum_square_diffs = fun_to_minimize(result)
    print(f"Range fit sum of squared differences: {sum_square_diffs}")
    assert sum_square_diffs < 1e-10, f"Fit error too large: {sum_square_diffs}"

def test_select_best_fit():
    """
    Test select_best_fit with a table from y = 3 + 5x + 4x^2.
    Provides linear, quadratic, and cubic functions, expecting quadratic to be best.
    """
    # Generate table
    x_values = np.linspace(1.0, 10.0, 10)  # 10 points
    y_values = 3.0 + 5.0 * x_values + 4.0 * x_values**2
    table = list(zip(x_values, y_values))

    # Define function objects
    def linear_fun(params, E0, stat_weight, x):
        a, b = params
        return a + b * x

    def quadratic_fun(params, E0, stat_weight, x):
        a, b, c = params
        return a + b * x + c * x**2

    def cubic_fun(params, E0, stat_weight, x):
        a, b, c, d = params
        return a + b * x + c * x**2 + d * x**3

    function_objects = [
        {'id': 'linear', 'function': linear_fun, 'num_params': 2},
        {'id': 'quadratic', 'function': quadratic_fun, 'num_params': 3},
        {'id': 'cubic', 'function': cubic_fun, 'num_params': 4}
    ]

    # Call select_best_fit
    best_id, best_params = select_best_fit(
        table=table,
        function_objects=function_objects,
        ionization_potential=1.0,
        stat_weight=2.0,
        method='powell',
        reject_bad_fits=False,
        bad_fit_threshold=1e-16
    )

    # Debugging
    print(f"Best function ID: {best_id}")
    print(f"Best parameters: {best_params}")

    # Assertions
    assert best_id == 'quadratic', f"Expected quadratic function, got {best_id}"
    assert best_params is not None, "Best parameters should not be None"
    assert len(best_params) == 3, "Should have 3 parameters for quadratic"
    assert all(np.isfinite(best_params)), "All parameters should be finite"
    assert np.isclose(best_params[0], 3.0, atol=1e-2), f"Parameter a should be 3.0, got {best_params[0]}"
    assert np.isclose(best_params[1], 5.0, atol=1e-2), f"Parameter b should be 5.0, got {best_params[1]}"
    assert np.isclose(best_params[2], 4.0, atol=1e-2), f"Parameter c should be 4.0, got {best_params[2]}"

    # Verify fit quality
    def fun_to_minimize(params):
        s = 0
        for x, y in table:
            v = quadratic_fun(params, 1.0, 2.0, x)
            s += (v - y) * (v - y)
        return s

    sum_square_diffs = fun_to_minimize(best_params)
    print(f"Best fit sum of squared differences: {sum_square_diffs}")
    assert sum_square_diffs < 1e-10, f"Fit error too large: {sum_square_diffs}"