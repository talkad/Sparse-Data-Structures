import os
import subprocess
from itertools import product
from multiprocessing import Process, Manager


save_dir = 'results'
local_dir = '/home/talkad/Desktop/data_structures'
base_dir = '/home/talkad/Desktop/data_structures/sparse_data_update'


class Execute:
    def __init__(self):
        # data struct parameters
        self.optimazation = 0
        self.num_materials = [2,4,8]
        self.nz_ratios = [0.01, 0.1, 0.3, 0.5, 1]
        self.structs = ['mat3d', 'linked_list', 'csr', 'csr_block'] # ['mat3d', 'linked_list','dynamic_array', 'dynamic_array_exist', 'csr3', 'csr2']


    def execute_params(self, struct):
        os.chdir(local_dir)
        imp_dir = os.path.join(save_dir, struct)
        os.makedirs(imp_dir, exist_ok = True)
        os.chdir(os.path.join(base_dir, struct))

        # compile
        execute = ['./script.sh', str(self.optimazation)]
        p=subprocess.Popen(execute)
        p.communicate()

        # execute
        for num_mats, nz_ratio in product(self.num_materials, self.nz_ratios):
            execute = ['./exe', str(num_mats), str(nz_ratio)]
            print(struct, ['./exe', str(num_mats), str(nz_ratio)])

            with open(os.path.join(local_dir, save_dir, struct, f'{num_mats}.txt'), 'a+') as f:
                p=subprocess.Popen(execute, stdout=f)
                p.communicate()


    def execute_struct_script(self, struct):
        print(struct)
        manager = Manager()
        return_dict = manager.dict()
        t = Process(target=self.execute_params, args=(struct,), daemon=True)

        t.start()
        t.join()


    def execute(self):
        for struct in self.structs:
            self.execute_struct_script(struct)


executor = Execute()
executor.execute()

