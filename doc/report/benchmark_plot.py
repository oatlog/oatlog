import matplotlib.pyplot as plt
import numpy as np

fixed_enodes=np.array([973, 1516, 50021])
fixed_oatlog=np.array([0.00043059, 0.00059043, 0.028676479])
fixed_egglog=np.array([0.005413959, 0.00702233, 0.189439364])

math_enodes=np.array([35, 69, 118, 208, 389, 784, 1576, 3160, 8113, 28303, 136446, 1047896,
                      15987528])
math_oatlog=np.array([4.522e-6, 9.82e-6, 1.8965e-5, 3.5945e-5, 7.0304e-5, 0.000151368, 0.000312157,
             0.000625671, 0.001363164, 0.004120975, 0.025372579, 0.252244785, 6.69198647])
math_egglog=np.array([0.000555115, 0.000688796, 0.00086984, 0.001078066, 0.001354427, 0.001813076,
             0.002610332, 0.004243919, 0.007541365, 0.015992291, 0.062065213, 0.440305378,
             8.181654906])

bool_enodes=np.array([44, 106, 241, 511, 727, 906, 1332, 2374, 5246, 15778, 77091, 854974,
                      24610667])
bool_oatlog=np.array([3.597e-6, 1.0493e-5, 2.635e-5, 7.1245e-5, 0.000136989, 0.000235751, 0.000352903,
             0.000572106, 0.001106922, 0.002782755, 0.012663299, 0.169890749, 151.989088928])
bool_egglog=np.array([0.000793247, 0.000966394, 0.001140428, 0.001533595, 0.002284799, 0.003658609,
             0.005163166, 0.006853989, 0.010354813, 0.01996156, 0.051558481, 0.331951569,
             159.766297003])

plt.rcParams["font.family"] = "NewComputerModernMath"

fig, ax_time = plt.subplots()
ax_speedup = ax_time.twinx()

ax_time.set_xscale("log")
ax_time.set_xlabel("e-nodes")
ax_time.set_yscale("log")
ax_time.set_ylabel("seconds")
ax_time.scatter(fixed_enodes, fixed_egglog, marker="*", c="red", label = "fuel math egglog")
ax_time.scatter(fixed_enodes, fixed_oatlog, marker="*", c="green", label = "fuel math oatlog")
ax_time.plot(math_enodes, math_egglog, c="red", label = "math egglog", linestyle="dashed")
ax_time.plot(math_enodes, math_oatlog, c="green", label = "math oatlog", linestyle="dashed")
ax_time.plot(bool_enodes, bool_egglog, c="red", label = "bool egglog", linestyle="dotted")
ax_time.plot(bool_enodes, bool_oatlog, c="green", label = "bool oatlog", linestyle="dotted")
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
