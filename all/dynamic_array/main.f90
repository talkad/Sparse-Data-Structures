program main
    use omp_lib
    use data_abstract_module
    use dynamic_array_module
    use mat_array_module
    use algorithm_module

    class(mat_array_t), pointer :: data_struct
    integer :: nx, ny, num_materials, i,j,m
    real(8), dimension(:,:,:), allocatable :: p
    real*8 :: time
    integer :: num_mats
    real :: nz_ratio
    character(len=32) :: arg

    ! call get_command_argument(1,arg)
    ! read(arg,*)  num_mats

    ! call get_command_argument(2,arg)
    ! read(arg,*)  nz_ratio

    nx = 10
    ny = 10
    num_materials = 2

    allocate(p(0:num_materials,0:nx,0:ny))
    call init_3d_deterministic(p, 1, nx, ny)
    ! call init_3d(p, nz_ratio, num_mats, nx, ny)

    ! do j=0, ny
    !     do i=0, nx
    !         do m=0, num_materials
    !             print*, p(m,i,j)
    !         end do 
    !     end do
    ! end do

    print*, '----------'
    data_struct => mat_array_constructor(p)
    print*, '----------'

    ! do j=0, ny
    !     do i=0, nx
    !         do m=0, num_materials
    !             print*, j,i,m
    !             print*, data_struct%get_item(m,i,j)
    !         end do 
    !     end do
    ! end do

    ! print*, nz_ratio
    ! time = omp_get_wtime()
    ! call intensive_algorithm(data_struct, nx, ny, num_materials)
    ! print*, 'func time', omp_get_wtime() - time

    ! time = omp_get_wtime()
    ! call intensive_algorithm_neighbors(data_struct, nx, ny, num_materials)
    ! print*, 'func time', omp_get_wtime() - time

    deallocate(p)
    ! print*, data_struct%idx_map
    ! print*, 'zzzzzzzzzzzzzz', size(data_struct%idx_map)
    deallocate(data_struct%idx_map)


end program


! nx * ny * 8 + (nx * ny * num_materials * 2*nz_ratio * 8)

! O0
!  Result intensive_algorithm   3015000.7500000000     
!  func time   11.785004254081286

!  Result intensive_algorithm_neighbors   15068996.250000000     
!  func time   34.401044472935610 
