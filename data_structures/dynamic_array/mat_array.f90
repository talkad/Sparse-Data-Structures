module mat_array_module
    use data_abstract_module
    use dynamic_array_module

    type, extends(data_abstract_t) :: mat_array_t
        private 
        type(dynamic_array_t), dimension(:,:), allocatable :: mat

        contains

            procedure :: get_item => get_item
            procedure :: add_item => add_item

    end type

    interface mat_array_t
        module procedure mat_array_constructor
    end interface

    contains


    function mat_array_constructor(matrix)
        implicit none
        type(mat_array_t), pointer :: mat_array_constructor
        real(8), dimension(:,:,:), intent(inout), allocatable :: matrix
        integer :: i, j, nx, ny, materials

        nx = size(matrix, dim=2) 
        ny = size(matrix, dim=3) 

        allocate(mat_array_constructor)
        allocate(mat_array_constructor%mat(0:nx,0:ny))

        do j = 0, ny-1
            do i = 0, nx-1
                ! print*, i, j, matrix(:,i,j)
                mat_array_constructor%mat(i,j) =  dynamic_array_constructor(matrix(:,i,j))
                ! print*, 'c', mat_array_constructor%mat(i-1,j-1)%values
            end do
        end do
    end function


    subroutine add_item(this, material_type, i, j, val)
        class(mat_array_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8) :: val

        ! irrelevant
    end subroutine


    function get_item(this, material_type, i, j)
        class(mat_array_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8) :: get_item

        get_item = this%mat(i,j)%get(material_type)
    end function get_item

end module