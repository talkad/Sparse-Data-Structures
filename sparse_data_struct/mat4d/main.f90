program main
    use omp_lib

    ! TAL !
    use sparse_struct_base_module   
    use mat4d_module                
    ! use mat_list_module             
    ! use csr_module                  
    use demo_algorithms_module
    ! TAL !
    implicit none


    ! TAL !
    integer, dimension(:,:,:,:), allocatable :: idx_map

    class(sparse_struct_base_t), pointer :: data_struct
    integer :: nx=300, ny=300, nz=300, num_mats=20
    integer, dimension(:), allocatable :: ms, is, js, ks
    real(8), dimension(:), allocatable :: vals
    real(8) :: nz_ratio=0.3, time
    integer :: size
    character(len=32) :: arg

    call get_command_argument(1,arg)
    read(arg,*)  num_mats

    call get_command_argument(2,arg)
    read(arg,*)  nz_ratio


    allocate(idx_map(1:num_mats,0:nx,0:ny,0:nz))
    idx_map(:,:,:,:) = -1

    size = nz_ratio*nx*ny*nz

    ! allocate
    allocate(ms(0:size-1))
    allocate(is(0:size-1))
    allocate(js(0:size-1))
    allocate(ks(0:size-1))
    allocate(vals(0:size-1))


    ! write(*,*) '--- main ---'
    data_struct => mat4d_constructor(num_mats, nx, ny, nz)
    ! data_struct => mat_list_constructor(nx, ny, nz)
    ! data_struct => sparse_constructor(idx_map)

    ! init data
    call advection(nx, ny, nz, num_mats, ms, is, js, ks, vals, nz_ratio)
    call data_struct%update_struct(ms, is, js, ks, vals)


    print*, nz_ratio
    time = omp_get_wtime()
    call intensive_algorithm(data_struct, nx, ny, nz, num_mats)
    print*, 'func time', omp_get_wtime() - time

    time = omp_get_wtime()
    call intensive_algorithm_mat(data_struct, nx, ny, nz, num_mats)
    print*, 'func time', omp_get_wtime() - time

    time = omp_get_wtime()
    call intensive_algorithm_neighbors(data_struct, nx, ny, nz, num_mats)
    print*, 'func time', omp_get_wtime() - time

    call advection(nx, ny, nz, num_mats, ms, is, js, ks, vals, nz_ratio)
    time = omp_get_wtime()
    call data_struct%update_struct(ms, is, js, ks, vals)
    print*, 'update time', omp_get_wtime() - time

    time = omp_get_wtime()
    call intensive_algorithm(data_struct, nx, ny, nz, num_mats)
    print*, 'func time', omp_get_wtime() - time


end program main

