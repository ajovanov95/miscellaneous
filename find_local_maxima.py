import numpy as np

import math
import time
import matplotlib.pyplot as plt

def f(x):
  if (x == 0):
    return 1
  else:
    return math.sin(x) / x


def find_local_maxima (f, sx, ex, inc_step):
  local_maxima = []
  increment    = (ex - sx) / inc_step

  start = time.time()

  for x in np.linspace (sx + inc_step, ex - inc_step, increment):
    try:
      if (f(x - inc_step) < f(x) and f(x) > f(x + inc_step)):
        local_maxima.append (x)
    except:
      pass

  end = time.time()

  print ("Minima finding parameteres were : start={0}, end={1}, inc_step={2}".format (sx, ex, inc_step))
  print ("It took {} seconds to find all local minima using a sequantial sliding rectangle method".format (end - start))

  return local_maxima, local_maxima [np.argmax (map (f, local_maxima))]


maxima, global_max = find_local_maxima (f, -200, 200, 0.01)

print ("Global maximum is : " + str (global_max))

plt.plot (np.arange(-200, 200), map (f, range(-200, 200)))

plt.scatter (maxima, map(f, maxima), color='g')

plt.scatter ([global_max], [f(global_max)], color='r')

plt.show ()
