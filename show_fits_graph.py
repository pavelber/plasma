import numpy as np
import matplotlib.pyplot as plt


a = 0.0
b = 0.0
c = 0.0
d = 0.0


def f(x):
    return (a + b / x + c / (x * x)) * (1 / (pow(x, (7.0 / 2.0 + d))))


if __name__ == '__main__':
    data_file = "C:\\work4\\db\\O\\1\\1_24_1.txt"
    start = 0.0
    end = 100.0
    coef = "1.695e-18  -1.122e-17   3.263e-17  -3.612e+00".split()
    a = float(coef[0])
    b = float(coef[1])
    c = float(coef[2])
    d = float(coef[3])



    # Generate x values
    x = np.linspace(start, end, 100)

    # Compute y values for the function
    y = f(x)

    # Plot the function
    plt.plot(x, y, label='f(x)')

    # Load the data from the file
    # Replace 'data.txt' with your filename
    # The file should contain two columns: x values and y values
    data = np.loadtxt(data_file, delimiter=",")

    # Plot the data from the file
    plt.plot(data[:, 0], data[:, 1], label='Data from file')

    # Add a legend
    plt.legend()

    # Show the plot
    plt.show()

