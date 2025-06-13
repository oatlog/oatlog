import matplotlib.pyplot as plt
import numpy as np

fixed_enodes=[973, 1516, 50021]
fixed_oatlog=[0.000437952, 0.000596766, 0.031503488]
fixed_egglog=[0.004613917, 0.005864082, 0.168089132]
math_enodes=[35, 69, 118, 208, 389, 784, 1576, 3160, 8113, 28303, 136446, 1047896, 15987528]
math_oatlog=[3.141e-6, 7.197e-6, 1.2975e-5, 2.3122e-5, 4.2043e-5, 8.6517e-5, 0.000214056, 0.000497214, 0.001115472, 0.003386883, 0.015350909, 0.174738665, 4.732847109]
math_egglog=[0.000533712, 0.000653275, 0.00081209, 0.000989971, 0.001223414, 0.001604959, 0.002256985, 0.003606554, 0.006269709, 0.013501082, 0.055338959, 0.436334115, 8.297412627]
bool_enodes=[44, 106, 241, 511, 727, 906, 1332, 2374, 5246, 15778, 77091, 854974, 24610667]
bool_oatlog=[3.232e-6, 6.715e-6, 1.6111e-5, 4.0023e-5, 8.7963e-5, 0.000151966, 0.000259034, 0.000463278, 0.000919643, 0.002234543, 0.009048608, 0.113358567, 140.036530806]
bool_egglog=[0.000755348, 0.000891244, 0.001062673, 0.001401833, 0.002032982, 0.003154532, 0.004338218, 0.005762356, 0.008699894, 0.016842371, 0.044512927, 0.3277195, 157.20419348]

fixed_enodes = np.array(fixed_enodes)
fixed_oatlog = np.array(fixed_oatlog)
fixed_egglog = np.array(fixed_egglog)
math_enodes = np.array(math_enodes)
math_oatlog = np.array(math_oatlog)
math_egglog = np.array(math_egglog)
bool_enodes = np.array(bool_enodes)
bool_oatlog = np.array(bool_oatlog)
bool_egglog = np.array(bool_egglog)

plt.rcParams["font.family"] = "NewComputerModernMath"

fig, ax_time = plt.subplots()
ax_speedup = ax_time.twinx()

ax_time.set_xscale("log")
ax_time.set_xlabel("e-nodes")
ax_time.set_yscale("log")
ax_time.set_ylabel("seconds")
ax_time.scatter(fixed_enodes, fixed_egglog, marker="*", c="red", label = "fuelN_math egglog")
ax_time.scatter(fixed_enodes, fixed_oatlog, marker="*", c="green", label = "fuelN_math oatlog")
ax_time.plot(math_enodes, math_egglog, c="red", label = "math egglog", linestyle="dashed")
ax_time.plot(math_enodes, math_oatlog, c="green", label = "math oatlog", linestyle="dashed")
ax_time.plot(bool_enodes, bool_egglog, c="red", label = "boolean_adder egglog", linestyle="dotted")
ax_time.plot(bool_enodes, bool_oatlog, c="green", label = "boolean_adder oatlog", linestyle="dotted")
ax_time.legend(loc="upper center")

ax_speedup.set_yscale("log")
ax_speedup.set_ylabel("speedup")
ax_speedup.scatter(fixed_enodes, fixed_egglog / fixed_oatlog, marker="*", c="blue", label = "fuel math speedup")
ax_speedup.plot(math_enodes, math_egglog / math_oatlog, c="blue", label = "math speedup",
                linestyle="dashed")
ax_speedup.plot(bool_enodes, bool_egglog / bool_oatlog, c="blue", label = "bool speedup",
                linestyle="dotted")
ax_speedup.legend(loc="lower center")

plt.savefig("benchmarks.svg")
