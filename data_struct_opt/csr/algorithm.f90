module algorithm_module
    use data_abstract_module
    use sparse_matrix_module


    contains


    ! subroutine init_3d_deterministic(matrix, mat, nx, ny)
    !     implicit none

    !     real(8), dimension(:,:,:), intent(inout), allocatable :: matrix
    !     integer, intent(in) :: nx, ny, mat
    !     integer :: i,j, ii, jj, mm
    !     integer, parameter :: block_size = 100
    !     matrix = 0.0

    !     do j=0, ny - block_size, block_size
    !         do i=0, nx - block_size, block_size

    !             do jj = j, j+block_size
    !                 do ii = i, i+block_size
    !                     do mm=0, mat
    !                         matrix(mm, ii, jj) = 20.0
    !                     end do 
    !                 end do
    !             end do

    !         end do
    !     end do
    
    ! end subroutine


    subroutine init_3d_deterministic(matrix, mat, nx, ny)
        implicit none

        real(8), dimension(:,:,:), intent(inout), allocatable :: matrix
        integer, intent(in) :: nx, ny, mat
        integer :: i,j, mm
        integer :: count = 1.0 
        matrix = 0.0

        do j=0, ny
            do i=0, nx
                do mm=0, mat
                    matrix(mm, i, j) = count
                    count = count + 1
                end do 
            end do
        end do
    
    end subroutine


    subroutine init_3d(matrix, nz_ratio, mat, nx, ny)
        implicit none

        real(8), dimension(:,:,:), intent(inout), allocatable :: matrix
        real, intent(in):: nz_ratio
        integer, intent(in) :: nx, ny, mat
        integer :: i,j,m

        
        do j=0, ny
            do i=0, nx
                do m=0, mat

                    if (rand(0) <= nz_ratio) then
                        matrix(m,i,j) = 20.0
                    else
                        matrix(m,i,j) = 0.0
                    end if 

                end do
            end do
        end do


    end subroutine




    subroutine intensive_algorithm(pressure_sum, nx, ny, num_materials)
        class(data_abstract_t), pointer, intent(in) :: pressure_sum
        integer, intent(in) :: nx,ny,num_materials

        integer :: i, j , m
        real(8) :: temp_sum = 0

        do j = 0, ny
            do i = 0, nx
                do m = 0, num_materials
                    temp_sum = temp_sum + 0.25d0 * pressure_sum%get_item(m, i, j)
                end do
            end do
        end do

        print*, 'Result intensive_algorithm', temp_sum

    end subroutine



    subroutine intensive_algorithm_neighbors(pressure_sum, nx, ny, num_materials)
        type(coo_matrix_t), pointer, intent(in) :: pressure_sum
        integer, intent(in) :: nx,ny,num_materials

        integer :: i, j , m
        real(8) :: temp
        real(8) :: curr, up, down, left, right

        do j = 1, ny-1
            do i = 1, nx-1
                do m = 0, num_materials
                    call pressure_sum%get_neighborhood(m,i,j, curr, up, down, left, right)
                    print*, 'aaaaa', j, i, m, curr, up, down, left, right
                    temp = 0.25d0 * ( curr + up + down + left + right)
                end do
            end do
        end do

        print*, 'Result intensive_algorithm_neighbors', temp

    end subroutine


end module