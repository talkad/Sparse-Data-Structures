program main
    use omp_lib

    ! TAL !
    use sparse_struct_base_module   
    ! use mat4d_module      
    ! use mat_array_module          
    ! use mat_list_module             
    use csr_module                  
    use demo_algorithms_module
    ! TAL !
    implicit none


    ! TAL !
    integer, dimension(:,:,:,:), allocatable :: idx_map_A, idx_map_B
    integer, dimension(:,:,:,:), allocatable :: idx_map_C

    class(sparse_struct_base_t), pointer :: A, B, C
    integer :: nx=200, ny=200, nz=200, num_mats=8
    integer, dimension(:), allocatable :: ms, is, js, ks
    real(8), dimension(:), allocatable :: vals
    real(8) :: nz_ratio=0.3, time
    integer :: size
    character(len=32) :: arg

    call get_command_argument(1,arg)
    read(arg,*)  num_mats

    call get_command_argument(2,arg)
    read(arg,*)  nz_ratio

    allocate(idx_map_A(1:num_mats,0:nx,0:ny,0:nz))
    idx_map_A(:,:,:,:) = -1
    allocate(idx_map_B(1:num_mats,0:nx,0:ny,0:nz))
    idx_map_B(:,:,:,:) = -1
    allocate(idx_map_C(1:num_mats,0:nx,0:ny,0:nz))
    idx_map_C(:,:,:,:) = -1

    size = nz_ratio*nx*ny*nz*num_mats

    ! allocate
    allocate(ms(0:size-1))
    allocate(is(0:size-1))
    allocate(js(0:size-1))
    allocate(ks(0:size-1))
    allocate(vals(0:size-1))


    ! write(*,*) '--- main ---'
    A => sparse_constructor(idx_map_A)
    B => sparse_constructor(idx_map_B)
    C => sparse_constructor(idx_map_C)


    ! init data
    call advection(nx, ny, nz, num_mats, ms, is, js, ks, vals, nz_ratio)
    call A%update_struct(ms, is, js, ks, vals)
    call B%update_struct(ms, is, js, ks, vals)


    print*, nz_ratio
    time = omp_get_wtime()
    call intensive_algorithm(A, B, C, nx, ny, nz, num_mats)
    print*, 'func time', omp_get_wtime() - time

    ! print*, A%total_sum(), C%total_sum()

    time = omp_get_wtime()
    call intensive_algorithm_mat(A, B, C, nx, ny, nz, num_mats)
    print*, 'func time', omp_get_wtime() - time

    time = omp_get_wtime()
    call intensive_algorithm_neighbors(A, C, nx, ny, nz, num_mats)
    print*, 'func time', omp_get_wtime() - time



end program main

