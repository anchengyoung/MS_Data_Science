import numpy as np
import matplotlib.pyplot as plt
import random
plt.close("all")
np.random.seed(2017)

def p_longest_streak(n, tries):
    all_attempts = []
    longest_flip = []
    for attempt in range(tries):
        flip_results = []
        streak = 0
        longest = 0
        for flip in range(n):
            result = random.randint(0, 1)
            if result == 1:
                flip_results.append(result)
                streak += 1
                if streak > longest:
                    longest = streak
            if result == 0:
                flip_results.append(result)
                streak = 0
        all_attempts.append(flip_results)
        longest_flip.append(longest)
    prob_list = []
    for times in range(n+1):
        probability = longest_flip.count(times)/len(longest_flip)
        prob_list.append(probability)
    ##for flip in range(n+1):
    ##    print("The probability of",flip,"heads in a row being the longest streak is ",prob_list[flip])
    return prob_list

p_longest_streak(200, 100000)

n_tries = [1e3,5e3,1e4,5e4,1e5]

n_vals = [5,200]

color_array = ['orange','darkorange','tomato','red', 'darkred', 'tomato', 'purple', 'grey', 'deepskyblue', 
               'maroon','darkgray','darkorange', 'steelblue', 'forestgreen', 'silver']
for ind_n in range(len(n_vals)):
    n = n_vals[ind_n]
    plt.figure(figsize=(20,5))
    for ind_tries in range(len(n_tries)):
        tries = n_tries[ind_tries]
        print("tries: " + str(tries))
        p_longest_tries = p_longest_streak(n, int(tries))
        plt.plot(range(n+1),p_longest_tries, marker='o',markersize=6,linestyle="dashed",lw=2,
                 color=color_array[ind_tries],
                 markeredgecolor= color_array[ind_tries],label=str(tries))
    plt.legend()
    
prob_list = p_longest_streak(200, 100000)

print("The probability that the longest streak of ones in a Bernoulli iid sequence of length 200 has length 8 or more is ")
print(1 - sum(prob_list[:8]))# Compute the probability and print it here

