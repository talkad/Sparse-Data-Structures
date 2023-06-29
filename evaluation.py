import os
import subprocess
from itertools import product
from multiprocessing import Process, Manager


save_dir = 'results'
local_dir = '/home/talkad_k/Desktop/Sparse-Data-Structures'
base_dir = '/home/talkad_k/Desktop/Sparse-Data-Structures/sparse_data_struct_inline'


class Execute:
    def __init__(self):
        # data struct parameters
        self.optimazation = 2
        self.num_materials = [2,4,8]
        self.nz_ratios = [0.01, 0.1, 0.3, 0.4, 0.5, 0.6]
        self.structs =  ['csr_update_pure']   #['backus', 'leeor4d', 'csr',  'csr_pure']     # ['backus', 'mat4d', 'leeor4d', 'csr']


    def execute_params(self, struct):
        os.chdir(local_dir)
        imp_dir = os.path.join(save_dir, struct)
        os.makedirs(imp_dir, exist_ok = True)
        os.chdir(os.path.join(base_dir, struct))

        # compile
        execute = ['./script.sh', str(self.optimazation)]
        print(execute)
        p=subprocess.Popen(execute)
        p.communicate()

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

