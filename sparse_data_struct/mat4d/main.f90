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

    class(sparse_struct_base_t), pointer :: A, B, C
    integer :: nx=220, ny=220, nz=220, num_mats=8
    integer, dimension(:), allocatable :: ms, is, js, ks
    real(8), dimension(:), allocatable :: vals
    real(8) :: nz_ratio=0.3, time
    integer :: size
    character(len=32) :: arg

    call get_command_argument(1,arg)
    read(arg,*)  num_mats

    call get_command_argument(2,arg)
    read(arg,*)  nz_ratio

    size = nz_ratio*nx*ny*nz*num_mats

    ! allocate
    allocate(ms(0:size-1))
    allocate(is(0:size-1))
    allocate(js(0:size-1))
    allocate(ks(0:size-1))
    allocate(vals(0:size-1))


    ! write(*,*) '--- main ---'
    A => mat4d_constructor(num_mats, nx, ny, nz)
    B => mat4d_constructor(num_mats, nx, ny, nz)
    C => mat4d_constructor(num_mats, nx, ny, nz)


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

! ./exe 5 0.3
! 0.29999999999999999     
! Result intensive_algorithm   12000000.000000000     
! func time  0.62171563971787691     
! Result intensive_algorithm_mat   12000000.000000000     
! func time  0.73784971516579390     
! Result intensive_algorithm_neighbors   80949295.000000000     
! func time   1.1633917605504394