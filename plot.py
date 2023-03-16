import matplotlib.pyplot as plt
import json
import pandas as pd


data = {}

result_dir = '/home/talkad/Desktop/data_structures/results'
data_structures = ['mat3d', 'linked_list', 'dynamic_array', 'csr2', 'csr3']
ratios = [0.01,0.1,0.3,0.5,1]
num_mats = [2,4,8]


# for struct in data_structures:
#     for mats in num_mats:

#         with open(f'{result_dir}/{struct}/{mats}_False.txt') as f:
#             log = f.readlines()

#             for line, ratio in zip([3,10,17,24,31],ratios):
#                 data[f'{struct}_{mats}_{ratio}'] = float(log[line-1].split()[-1])

# print(data)
        

# with open('intensive.json', 'w') as f:
#     json.dump(data, f, indent=2, separators=(',', ': '))



algorithms = ['intensive', 'intensive_mats', 'intensive_neighbors']

for algorithm in algorithms:
    with open(f'{algorithm}.json', 'r') as f:
        algo = json.load(f)
    
    for mats in num_mats:
        
        ys = {}
        for data in data_structures:
            ys[data] = []

            for ratio in ratios:
                ys[data].append(algo[f'{data}_{mats}_{ratio}'])

        # plot
        plt.clf()
        for label, y in ys.items():
            plt.title(f'{algorithm} with {mats} materials')
            plt.xlabel('ratio')
            plt.ylabel('execution time (sec)')
            plt.plot(ratios, y, label=label)
            plt.legend()
            plt.savefig(f'plots/{algorithm}_{mats}.jpeg')

    




            
