import afa
import entropy
import numpy as np
import pandas

# Read data
cop_x = pandas.read_csv("Data Files\COP X.csv", sep = ',')
cop_y = pandas.read_csv("Data Files\COP Y.csv", sep = ',')

# Convert to Numpy for sanity
cop_x_np = cop_x.as_matrix()
cop_y_np = cop_y.as_matrix()

# Set parameters
r = 0.15
max_tau = 34

# Set output frames
mse_x = np.zeros((cop_x.shape[0], max_tau))
mse_y = np.zeros((cop_y.shape[0], max_tau))
mse_x_detrended = np.zeros((cop_x.shape[0], max_tau))
mse_y_detrended = np.zeros((cop_y.shape[0], max_tau))

# Run MSE analysis
for i in range(cop_x_np.shape[0]):
    mse_x[i] = entropy.multiscale_entropy(time_series = cop_x_np[i], tau = max_tau, r = r, status = True)
    mse_y[i] = entropy.multiscale_entropy(time_series = cop_y_np[i], tau = max_tau, r = r, status = True)

    dx, x = afa.detrending_method(cop_x_np[i], seg_len = 129, fit_order = 2)
    dy, y = afa.detrending_method(cop_y_np[i], seg_len = 129, fit_order = 2)

    mse_x_detrended[i] = entropy.multiscale_entropy(time_series=dx, tau=max_tau, r=r, status=True)
    mse_y_detrended[i] = entropy.multiscale_entropy(time_series=dy, tau=max_tau, r=r, status=True)

    print("Row", i, "completed", sep=" ")

# Write MSE frames to CSV for further analysis
mse_x_df = pandas.DataFrame(mse_x)
mse_y_df = pandas.DataFrame(mse_y)
mse_x_dt_df = pandas.DataFrame(mse_x_detrended)
mse_y_dt_df = pandas.DataFrame(mse_y_detrended)

mse_x_df.to_csv("Data Files\MSE X.csv", header=False)
mse_y_df.to_csv("Data Files\MSE Y.csv", header=False)
mse_x_dt_df.to_csv("Data Files\Detrended X MSE.csv", header=False)
mse_y_dt_df.to_csv("Data Files\Detrended Y MSE.csv", header=False)
