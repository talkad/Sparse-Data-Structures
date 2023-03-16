module algorithm_module
    use data_abstract_module


    contains


    subroutine init_3d(matrix, nz_ratio, mat, nx, ny)
        implicit none

        real(8), dimension(:,:,:), intent(inout), allocatable :: matrix
        real, intent(in):: nz_ratio
        integer, intent(in) :: nx, ny, mat
        integer :: i,j,m
        matrix = 0.0

        print*, mat
        
        do j=0, ny
            do i=0, nx

                if (rand(0) <= nz_ratio) then
                    do m=0, mat ! int(mat*rand(0))                   
                        matrix(m,i,j) = 1.0
                    end do
                else                        ! vacuum is a material
                    matrix(0,i,j) = 1.0
                end if 

            end do
        end do

    end subroutine


    subroutine init_3d_deterministic(matrix, mat, nx, ny)
        implicit none

        real(8), dimension(:,:,:), intent(inout), allocatable :: matrix
        integer, intent(in) :: nx, ny, mat
        integer :: i,j, ii, jj, mm, count
        integer, parameter :: block_size = 2
        matrix = 0.0
        ! count = 0

        do j=0, ny - block_size, block_size
            do i=j, j

                do jj = j, j+block_size
                    do ii = i, i+block_size
                        do mm=0, mat
                            matrix(mm, ii, jj) = 1.0
                            ! count = count + 1
                        end do 
                    end do
                end do

            end do
        end do

        ! print*, "num of non-zero", count
    end subroutine

    ! subroutine init_3d_deterministic(matrix, mat, nx, ny)
    !     implicit none

    !     real(8), dimension(:,:,:), intent(inout), allocatable :: matrix
    !     integer, intent(in) :: nx, ny, mat
    !     integer :: i,j, mm
    !     integer :: count = 1.0 
    !     matrix = 0.0

    !     do j=0, ny
    !         do i=0, nx
    !             do mm=0, mat
    !                 matrix(mm, i, j) = count
    !                 count = count + 1
    !             end do 
    !         end do
    !     end do
    
    ! end subroutine

    subroutine intensive_algorithm_neighbors(pressure_sum, nx, ny, num_materials)
        class(data_abstract_t), pointer, intent(in) :: pressure_sum
        integer, intent(in) :: nx,ny,num_materials

        integer :: i, j , m
        real(8) :: temp
        real(8) :: curr, up, down, left, right

        do j = 1, ny-1
            do i = 1, nx-1
                do m = 0, num_materials
                    ! print*, '-----------------'
                    ! print*, j,i,m
                    call pressure_sum%get_neighbors(m,i,j, curr, up, down, left, right)
                    ! print*, curr !, up, down, left, right
                    temp = temp + 0.25d0 * ( curr + up + down + left + right)
                end do
            end do
        end do

        print*, 'Result intensive_algorithm_neighbors', temp
    end subroutine


    subroutine intensive_algorithm(pressure_sum, nx, ny, num_materials)
        class(data_abstract_t), pointer, intent(in) :: pressure_sum
        integer, intent(in) :: nx,ny,num_materials

        integer :: i, j , m
        real(8) :: temp_sum = 0

        print*, ny, ny, num_materials

        do j = 0, ny
            do i = 0, nx
                do m = 0, num_materials
                        ! print*, j,i,m
                        ! print*, pressure_sum%get_item(m, i, j)
                        temp_sum = temp_sum + 0.25d0 * pressure_sum%get_item(m, i, j)
                end do
            end do
        end do

        print*, 'Result intensive_algorithm', temp_sum
    end subroutine


    subroutine intensive_algorithm_mat(pressure_sum, nx, ny, num_materials)
        class(data_abstract_t), pointer, intent(in) :: pressure_sum
        integer, intent(in) :: nx,ny,num_materials

        integer :: i, j , m
        real(8) :: temp_sum = 0

        ! print*, ny, ny, num_materials

        do m = 0, num_materials
            do j = 0, ny
                do i = 0, nx
                        ! print*, m, i, j
                        ! print*, m, i, j,pressure_sum%get_item(m, i, j)
                        temp_sum = temp_sum + 0.25d0 * pressure_sum%get_item(m, i, j)
                end do
            end do
        end do

        print*, 'Result intensive_algorithm', temp_sum
    end subroutine

end module