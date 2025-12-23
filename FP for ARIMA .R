# ==========================================
# 1. Install & Load Packages
# ==========================================
# install.packages(c("forecast", "tseries", "ggplot2"))
library(forecast)
library(tseries)
library(ggplot2)

# ==========================================
# 2. Load & Preprocess Data
# ==========================================
data <- read.csv("E:/Semester 6/SML-A/BRAZIL.csv", sep = ";")

# Ubah angka dan tanggal
data$GWh <- as.numeric(gsub(",", ".", data$GWh))
data$date <- as.Date(data$date, format = "%d/%m/%Y")

# Pengecekan Missing Value sebelum dihapus
missing_count <- sum(is.na(data$GWh))
cat("Jumlah missing value pada kolom GWh:", missing_count, "\n")

# Jika ada missing value, hapus baris tersebut
if (missing_count > 0) {
  data <- na.omit(data)
  cat("Missing value telah dihapus. Data sekarang memiliki", nrow(data), "baris.\n")
} else {
  cat("Tidak ditemukan missing value pada data.\n")
}

# Buat time series object (harian)
ts_data <- ts(data$GWh, frequency = 365)


# ==========================================
# 3. Periksa Stasioneritas (Original Data)
# ==========================================
adf_result <- adf.test(ts_data)
cat("ADF p-value (Original):", adf_result$p.value, "\n")

# ==========================================
# 4. Differencing (Jika Tidak Stasioner)
# ==========================================
if (adf_result$p.value > 0.05) {
  ts_diff <- diff(ts_data)
  adf_diff <- adf.test(ts_diff)
  cat("ADF p-value (Setelah Differencing):", adf_diff$p.value, "\n")
  
  # Visualisasi setelah differencing
  autoplot(ts_diff) + ggtitle("Data Setelah Differencing") +
    ylab("GWh") + xlab("Tanggal")
  ggAcf(ts_diff) + ggtitle("ACF Data Differencing")
  ggPacf(ts_diff) + ggtitle("PACF Data Differencing")
}

# ==========================================
# 5. Split Train & Test Data
# ==========================================
n <- length(ts_data)
n_train <- floor(0.83 * n)
n_test <- n - n_train

ts_train <- ts(ts_data[1:n_train], start = start(ts_data), frequency = frequency(ts_data))
ts_test <- ts(ts_data[(n_train + 1):n], start = time(ts_data)[n_train + 1], frequency = frequency(ts_data))

# ==========================================
# 6. ARIMA (3,1,4)
# ==========================================
lambda <- BoxCox.lambda(ts_train)
lambda
model_arima_fixed <- Arima(ts_train, order = c(3, 1, 4), lambda = lambda)
summary(model_arima_fixed)

# Evaluasi Model
cat("AIC ARIMA (3,1,4):", AIC(model_arima_fixed), "\n")

# Forecast pada test data
forecast_test_fixed <- forecast(model_arima_fixed, h = n_test)

# Hitung evaluasi: MAPE, MAE, RMSE
y_test_pred <- as.numeric(forecast_test_fixed$mean)
y_test_actual <- as.numeric(ts_test)

valid_idx <- !is.na(y_test_pred) & !is.na(y_test_actual)
y_test_pred <- y_test_pred[valid_idx]
y_test_actual <- y_test_actual[valid_idx]

mape <- mean(abs((y_test_actual - y_test_pred) / y_test_actual)) * 100
mae <- mean(abs(y_test_actual - y_test_pred))
rmse <- sqrt(mean((y_test_actual - y_test_pred)^2))

cat("MAPE Test:", round(mape, 4), "%\n")
cat("MAE Test:", round(mae, 4), "\n")
cat("RMSE Test:", round(rmse, 4), "\n")

# ==========================================
# 7. Visualisasi Forecast
# ==========================================
autoplot(ts_data) +
  autolayer(forecast_test_fixed, series = "Forecast (ARIMA (3,1,4))", PI = FALSE) +
  autolayer(ts_test, series = "Test Data") +
  ggtitle("Forecast vs Test Data (ARIMA (3,1,4))") +
  ylab("GWh") + xlab("Tanggal")

# ==========================================
# 8. Final Forecast (Full Data)
# ==========================================
model_final <- Arima(ts_data, order = c(3, 1, 4), lambda = lambda)
forecast_final <- forecast(model_final, h = 30)

# Visualisasi Final Forecast
autoplot(forecast_final) +
  ggtitle("Forecast Konsumsi Minyak 30 Hari ke Depan (ARIMA (3,1,4))") +
  ylab("GWh") + xlab("Tanggal")

# ==========================================
# 9. Cek Residual
# ==========================================
# Residual Analysis
checkresiduals(model_arima_fixed)

# Uji Normalitas Residual
shapiro_test <- shapiro.test(residuals(model_arima_fixed))
cat("Shapiro-Wilk Test p-value:", shapiro_test$p.value, "\n")
if (shapiro_test$p.value > 0.05) {
  cat("Residuals mengikuti distribusi normal.\n")
} else {
  cat("Residuals tidak mengikuti distribusi normal.\n")
}

# Visualisasi Residual
autoplot(residuals(model_arima_fixed)) +
  ggtitle("Residual Plot (ARIMA (3,1,4))") +
  ylab("Residual") + xlab("Waktu")

# Uji Autocorrelation
Box.test(residuals(model_arima_fixed), lag = 10, type = "Ljung-Box")
cat("Ljung-Box Test (p-value):", Box.test(residuals(model_arima_fixed), lag = 10)$p.value, "\n")

if (Box.test(residuals(model_arima_fixed), lag = 10)$p.value > 0.05) {
  cat("Residuals tidak memiliki autocorrelation signifikan.\n")
} else {
  cat("Residuals memiliki autocorrelation signifikan.\n")
}