module algorithm_module
    use data_abstract_module


    contains


    subroutine init_3d(matrix, nz_ratio, mat, nx, ny)
        implicit none

        real(8), dimension(:,:,:), intent(inout), allocatable :: matrix
        real, intent(in):: nz_ratio
        integer, intent(in) :: nx, ny, mat
        integer :: i,j,m
        real(8) :: val


        
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



    subroutine intensive_algorithm_neighbors(pressure_sum, nx, ny, num_materials)
        class(data_abstract_t), pointer, intent(in) :: pressure_sum
        integer, intent(in) :: nx,ny,num_materials

        integer :: i, j , m
        real(8) :: temp

        do j = 1, ny
            do i = 1, nx
                do m = 1, num_materials
                        temp = 0.25d0 * (               &
                        - pressure_sum%get_item(m, i - 1, j) &
                        + pressure_sum%get_item(m, i    , j) &
                        + pressure_sum%get_item(m, i    , j) &
                        - pressure_sum%get_item(m, i - 1, j))
                end do
            end do
        end do

    end subroutine


    subroutine intensive_algorithm(pressure_sum, nx, ny, num_materials)
        class(data_abstract_t), pointer, intent(in) :: pressure_sum
        integer, intent(in) :: nx,ny,num_materials

        integer :: i, j , m
        real(8) :: temp_sum

        do j = 1, ny
            do i = 1, nx
                do m = 1, num_materials
                        temp_sum = temp_sum + 0.25d0 * pressure_sum%get_item(m, i, j)
                end do
                temp_sum = 0
            end do
        end do

    end subroutine


end module