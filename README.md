<h1 align="center">🌧️ Weather Precipitation Predictor 🌤️</h1>

<p align="center">
  <img src="https://img.shields.io/badge/Project-Type:ML-blue?style=for-the-badge" />
  <img src="https://img.shields.io/badge/Language-Python-yellow?style=for-the-badge" />
  <img src="https://img.shields.io/badge/Model-KNN%20|%20DecisionTree-green?style=for-the-badge" />
  <img src="https://img.shields.io/badge/Data-Cleaned%20%26%20Visualized-purple?style=for-the-badge" />
</p>

---

## 📌 Project Summary

**Weather Precipitation Predictor** is a machine learning project designed to predict **precipitation levels (in mm)** based on historical meteorological data. Instead of binary rain prediction, this project **quantifies** how much precipitation can be expected using regression and classification models.

---

## 🧠 Key Features

- 🔍 **Data Preprocessing** and Feature Engineering  
- 📊 **Data Visualization** (heatmaps, distributions, etc.)  
- 🧪 **Model Training** using:
  - K-Nearest Neighbors (KNN)
  - Decision Tree Regressor
- 🧼 Handling missing values, encoding categorical features  
- ✅ Model Evaluation using Confusion Matrix & Accuracy Score  
- 📈 Predicts **actual precipitation values** rather than "rain or no rain"

---

## 📂 Dataset Overview

| Feature              | Description                                 |
|----------------------|---------------------------------------------|
| Temperature (C)      | Temperature at the time of measurement      |
| Humidity             | Relative humidity (%)                       |
| Wind Speed (km/h)    | Wind speed at ground level                  |
| Visibility (km)      | How far you can see                         |
| Pressure (millibars) | Atmospheric pressure                        |
| Summary              | Weather summary (encoded)                   |
| Precipitation (mm)   | **Target** – How much precipitation fell    |

📁 Dataset: `weatherHistory.csv`  
📦 Size: ~96,000 records

---

## 📊 Visualizations Included

- 🌡️ **Correlation Heatmap**
- 📘 **Confusion Matrix (KNN)**
- 🌲 **Decision Tree Model Score**
- 🌦️ **Actual vs. Predicted Precipitation Scatter Plot**

<p align="center">
  <img src="https://upload.wikimedia.org/wikipedia/commons/1/10/Heatmap.png" width="500"/>
</p>

---

## 🚀 Tech Stack

| Tool        | Purpose                           |
|-------------|-----------------------------------|
| `Python`    | Core programming language         |
| `Pandas`    | Data manipulation                 |
| `NumPy`     | Numerical operations              |
| `Matplotlib`/`Seaborn` | Visualization         |
| `Scikit-learn` | ML models, preprocessing, metrics |
| `Git & GitHub` | Version control & hosting     |

---

## 🧪 Model Evaluation

### 🔹 KNN Classifier
- Trained on scaled data
- Accuracy visualized using confusion matrix

### 🔸 Decision Tree Regressor
- Predicts continuous precipitation values
- Score displayed directly in output

---

## 💻 How to Run Locally

```bash
# Clone the repository
git clone https://github.com/Yaseenshaik17/Weather_Predictor_R.git

# Navigate into the project directory
cd Weather_Predictor_R

# Install dependencies (optional virtualenv)
pip install -r requirements.txt

# Run the Python script
python ProjectCode.py
