program main
    use omp_lib
    use data_abstract_module
    use sparse_matrix_module
    use algorithm_module

    class(data_abstract_t), pointer :: data_struct
    integer :: nx, ny, num_materials, i,j,m
    real(8), dimension(:,:,:), allocatable :: p
    real*8 :: time
    integer, parameter :: N = 5 
    integer, dimension(:), allocatable :: indx
    integer :: idx , a,b

    ! allocate(indx(0:N))

    ! indx = [1,2,2,2,2, 4, 4, 4,4,4,4,4,4]
    ! print*, indx

    ! idx = idx_exists(indx, 2)

    ! print*, idx


    ! call get_boundries(a, b, indx, idx)
    ! print*,a,b


    nx = 4
    ny = 4
    num_materials = 2


    allocate(p(0:num_materials,0:nx,0:ny))
    call init_3d(p, 0.1, num_materials, nx, ny)
    
    ! init data structure
    data_struct => sparse_constructor(p)
    ! print*, 'done'

    ! print*, data_struct%get_item(0,0,0)

    ! do j = 0, ny
    !     do i  = 0, nx
    !         do m = 0, num_materials
    !             print*, data_struct%get_item(m,i,j)
    !         end do
    !     end do
    ! end do
    
    print*, '-------'
    ! time = omp_get_wtime()
    ! call intensive_algorithm(data_struct, nx, ny, num_materials)
    ! print*, 'func time', omp_get_wtime() - time
    deallocate(p)

end program
