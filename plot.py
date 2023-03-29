import matplotlib.pyplot as plt
import json
import pandas as pd
import os
from tqdm import tqdm


data = {}

result_dir = '/home/talkad/Desktop/data_structures/results'
data_structures = ['mat3d', 'linked_list', 'csr'] # ['mat3d', 'csr3', 'csr_block_4', 'csr_block_8', 'csr_block_16', 'csr_block_64', 'csr_block_4_inline', 'csr_block_8_inline', 'csr_block_16_inline', 'csr_block_64_inline']


ratios = [0.01,0.1,0.3,0.5,1]
num_mats = [2,4,8]
algorithms = ['jim', 'mji', 'stencil', 'update']  # ['intensive', 'intensive_mats', 'intensive_neighbors'] 


for algo, idx in tqdm(zip(algorithms, [-1,1,3,4])):
    for struct in data_structures:
        for mats in num_mats:

            with open(f'{result_dir}/{struct}/{mats}.txt') as f:
                log = f.readlines()

                for line, ratio in zip([3,13,23,33,43],ratios):
                    data[f'{struct}_{mats}_{ratio}'] = float(log[line+idx].split()[-1])

    print(data)
    
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

    




            
