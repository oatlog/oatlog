import matplotlib.pyplot as plt
import numpy as np

steps = np.arange(12)

math_oatlog = np.array([
    6.0900, 17.055, 32.492, 61.145, 122.53, 253.68,
    539.74, 1119.9, 2654.3, 8680.4, 53112.0, 563930.0
])  # µs

math_egglog = np.array([
    550.71, 688.83, 871.97, 1084.5, 1369.2, 1841.0,
    2663.9, 4348.6, 7684.5, 16224.0, 63135.0, 448770.0
])  # µs

adder_oatlog = np.array([
    4.8048, 19.588, 49.379, 120.61, 226.53, 370.08,
    530.71, 919.43, 2023.9, 5578.7, 25410.0, 478830.0
])  # µs

adder_egglog = np.array([
    781.55, 936.01, 1130.3, 1543.7, 2298.8, 3719.9,
    5197.1, 6955.6, 10596.0, 20219.0, 52227.0, 406150.0
])  # µs

plt.figure(figsize=(10, 6))
#plt.yscale("log")

plt.plot(steps, math_oatlog, marker='o', label="math - oatlog", color='tab:blue')
plt.plot(steps, math_egglog, marker='o', label="math - egglog", color='tab:red')

plt.plot(steps, adder_oatlog, marker='s', label="boolean-adder - oatlog", color='tab:cyan')
plt.plot(steps, adder_egglog, marker='s', label="boolean-adder - egglog", color='tab:orange')

plt.xlabel("Steps")
plt.ylabel("Time (µs)")
plt.title("Performance vs. Steps (log scale time)")
plt.legend()
plt.grid(True, which="both", linestyle='--', linewidth=0.5)

plt.tight_layout()
plt.show()
