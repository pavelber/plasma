import numpy as np
from scipy.optimize import minimize


import numpy as np
from scipy.optimize import minimize

def create_fits(
        table,
        approximation_fun,
        ionization_potential,
        stat_weight,
        initial_params=None,
        bounds=None,
        method='powell',
        reject_bad_fits=True,
        bad_fit_threshold=1e-16
):
    """
    Create fitting parameters using a table of (x, y) values and an approximation function.

    Args:
        table: List of (x, y) tuples or two arrays [x_values], [y_values].
        approximation_fun (callable): Approximation function with signature
                                     (params, E0, stat_weight, x) -> float.
        ionization_potential (float): Ionization potential for the transition.
        stat_weight (float): Statistical weight for the transition.
        initial_params (list or None): Initial guess for fitting parameters.
                                      If None, uses first y-value from table for all parameters.
        bounds (list of tuples or None): List of (min, max) bounds for each parameter.
                                        Use None for unbounded min/max (e.g., (0, None) for x >= 0).
        method (str): Optimization method for scipy.optimize.minimize.
        reject_bad_fits (bool): If True, reject fits with high error.
        bad_fit_threshold (float): Threshold for determining a bad fit.

    Returns:
        tuple: (fitted_params, sum_square_diffs) where fitted_params is a tuple of parameters
               or None, and sum_square_diffs is the sum of squared differences or np.inf.
    """
    # Parse table
    if isinstance(table, (list, tuple)) and all(isinstance(t, (list, tuple)) for t in table):
        x_values = np.array([t[0] for t in table], dtype=float)
        y_values = np.array([t[1] for t in table], dtype=float)
    elif isinstance(table, (list, tuple)) and len(table) == 2:
        x_values = np.array(table[0], dtype=float)
        y_values = np.array(table[1], dtype=float)
    else:
        raise ValueError("Table must be a list of (x, y) tuples or two arrays [x_values], [y_values]")

    # Validate table
    if len(x_values) < 2 or len(x_values) != len(y_values):
        raise ValueError("Table must contain at least 2 points with matching x and y values")
    if np.any(~np.isfinite(x_values)) or np.any(~np.isfinite(y_values)):
        raise ValueError("Table contains non-finite values")

    # Define objective function
    def fun_to_minimize(params):
        s = 0
        for x, y in zip(x_values, y_values):
            try:
                v = approximation_fun(params, ionization_potential, stat_weight, x)
                s += (v - y) * (v - y)
            except ValueError:
                return np.inf  # Penalize invalid parameters
        return s

    # Determine initial parameters
    if initial_params is None:
        zero_v = y_values[0] if len(y_values) > 0 else 1.0
        num_params = 4  # Default
        x0 = np.array([zero_v] * num_params)
    else:
        x0 = np.array(initial_params)
        num_params = len(initial_params)

    # Clip initial parameters to bounds, handling None
    if bounds is not None:
        if len(bounds) != num_params:
            raise ValueError(f"Bounds length ({len(bounds)}) must match number of parameters ({num_params})")
        # Replace None with inf/-inf for np.clip
        clip_min = [b[0] if b[0] is not None else -np.inf for b in bounds]
        clip_max = [b[1] if b[1] is not None else np.inf for b in bounds]
        x0 = np.clip(x0, clip_min, clip_max)

    # Perform optimization
    try:
        res = minimize(
            fun_to_minimize,
            x0,
            method=method,
            bounds=bounds  # scipy.optimize.minimize handles None correctly
        )
    except ValueError as e:
        print("X", end="")
        return None, np.inf

    if res.success:
        fitted_params = res.x
        sum_square_diffs = fun_to_minimize(fitted_params)
        if reject_bad_fits and sum_square_diffs > bad_fit_threshold:
            print("+", end="")
            return None, np.inf
        return tuple(fitted_params), sum_square_diffs
    else:
        print("X", end="")
        return None, np.inf
def create_fits_from_range(
        cross_cut_function,
        approximation_fun,
        ionization_potential,
        stat_weight,
        initial_params=None,
        bounds=None,
        start_e=1.0,
        end_e=100.0,
        step_e=1.0,
        method='powell',
        reject_bad_fits=True,
        bad_fit_threshold=1e-16
):
    """
    Wrapper for create_fits that generates a table from an energy function over a range.
    """
    x_values = np.arange(start_e, end_e + step_e, step_e)
    try:
        y_values = np.array([cross_cut_function(x) for x in x_values], dtype=float)
    except Exception as e:
        print(f"Failed to evaluate energy_function: {e}")
        return None, np.inf

    table = list(zip(x_values, y_values))
    return create_fits(
        table=table,
        approximation_fun=approximation_fun,
        ionization_potential=ionization_potential,
        stat_weight=stat_weight,
        initial_params=initial_params,
        bounds=bounds,
        method=method,
        reject_bad_fits=reject_bad_fits,
        bad_fit_threshold=bad_fit_threshold
    )

def select_best_fit(
        table,
        function_objects,
        ionization_potential,
        stat_weight,
        method='powell',
        reject_bad_fits=True,
        bad_fit_threshold=1e-16
):
    """
    Select the best fitting function from a list of function objects for a given table.
    """
    best_id = None
    best_params = None
    best_sum_square_diffs = np.inf

    for func_obj in function_objects:
        func_id = func_obj['id']
        func = func_obj['function']
        num_params = func_obj['num_params']
        initial_params = func_obj.get('initial_params', None)
        bounds = func_obj.get('bounds', None)  # Get bounds from function object

        # Generate initial parameters
        if isinstance(table, (list, tuple)) and all(isinstance(t, (list, tuple)) for t in table):
            y0 = table[0][1]
        else:
            y0 = table[1][0]

        if initial_params is None:
            initial_params = [y0] * num_params

        # Fit the function
        params, sum_square_diffs = create_fits(
            table=table,
            approximation_fun=func,
            ionization_potential=ionization_potential,
            stat_weight=stat_weight,
            initial_params=initial_params,
            bounds=bounds,
            method=method,
            reject_bad_fits=reject_bad_fits,
            bad_fit_threshold=bad_fit_threshold
        )

        # Update best fit if this is better
        if params is not None and sum_square_diffs < best_sum_square_diffs:
            best_id = func_id
            best_params = params
            best_sum_square_diffs = sum_square_diffs

        print(f"Function {func_id}: sum_square_diffs={sum_square_diffs}")

    return best_id, best_params