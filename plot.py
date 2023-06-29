import matplotlib.pyplot as plt
import json
import pandas as pd
import os
from tqdm import tqdm


data = {}

result_dir = '/home/talkad_k/Desktop/Sparse-Data-Structures/results'
data_structures = ['backus', 'leeor4d', 'csr',  'csr_pure', 'csr_update_pure']

ratios = [0.01,0.1,0.3,0.4,0.5,0.6]
num_mats = [2,4,8]
algorithms = ['kjim', 'mkji', 'stencil']

for algo, idx in tqdm(zip(algorithms, [0,2,4])):
    for struct in data_structures:
        for mats in num_mats:

            with open(f'{result_dir}/{struct}/{mats}.txt') as f:
                log = f.readlines()
                for line, ratio in zip([2,9,16,23,30,37],ratios):
                    data[f'{struct}_{mats}_{ratio}'] = float(log[line+idx].split()[-1])

    with open(f'{algo}.json', 'w') as f:
        json.dump(data, f, indent=2, separators=(',', ': '))


os.makedirs('plots', exist_ok = True)

for algorithm in tqdm(algorithms):
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
            if 'inline' in label:
                plt.plot(ratios, y, label=label, linestyle='dashed')
            else:
                plt.plot(ratios, y, label=label)
            plt.legend()
            plt.savefig(f'plots/{algorithm}_{mats}.jpeg')

    




            
