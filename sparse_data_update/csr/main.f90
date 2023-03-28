program main
    use omp_lib
    use data_abstract_module
    use sparse_matrix_module
    use algorithm_module

    type(coo_matrix_t), pointer :: data_struct
    integer :: nx, ny, num_materials, i,j,m
    real(8), dimension(:,:,:), allocatable :: p
    real*8 :: time
    integer, dimension(:,:,:), allocatable :: idx_map
    integer :: idx , a,b
    integer :: num_mats
    real :: nz_ratio
    character(len=32) :: arg

    integer, dimension(:), allocatable :: ms, is, js
    real(8), dimension(:), allocatable :: vals

    call get_command_argument(1,arg)
    read(arg,*)  num_mats

    call get_command_argument(2,arg)
    read(arg,*)  nz_ratio

    nx = 5000
    ny = 5000
    num_materials = 20

    allocate(p(0:num_materials,0:nx,0:ny))
    ! call init_3d_deterministic(p, 2, nx, ny)
    allocate(idx_map(0:num_materials,0:nx,0:ny))
    call init_3d(p, nz_ratio, num_mats, nx, ny)

    ! print*, '-------'
    data_struct => sparse_constructor(p,idx_map)
    
    ! print*, '-------'

    print*, nz_ratio
    time = omp_get_wtime()
    call intensive_algorithm(data_struct, nx, ny, num_materials)
    print*, 'func time', omp_get_wtime() - time

    ! time = omp_get_wtime()
    ! call intensive_algorithm_mat(data_struct, nx, ny, num_materials)
    ! print*, 'func time', omp_get_wtime() - time

    ! time = omp_get_wtime()
    ! call intensive_algorithm_neighbors(data_struct, nx, ny, num_materials)
    ! print*, 'func time', omp_get_wtime() - time

    call advection(nx, ny, num_materials, ms, is, js, vals)
    time = omp_get_wtime()
    call data_struct%update_struct(ms, is, js, vals)
    print*, 'update time', omp_get_wtime() - time

    time = omp_get_wtime()
    call intensive_algorithm(data_struct, nx, ny, num_materials)
    print*, 'func time', omp_get_wtime() - time

    deallocate(p)
    deallocate(data_struct%idx_map)

end program

! 2 * nx * ny * num_materials * nz_ratio (8+ 3*4)
! 188,743,660

! O0
!  Result intensive_algorithm   3015000.7500000000
!  func time   7.0705450670793653

!  func time   79.150903295027092 
