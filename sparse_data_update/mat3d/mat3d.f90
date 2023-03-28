module mat3d_module
    use data_abstract_module

    type, extends(data_abstract_t) :: mat3d_t
        real(8), dimension(:,:,:), allocatable :: matrix
        contains
            procedure :: get_item => get_item
            procedure :: get_neighbors => get_neighbors
            procedure :: add_item => add_item
            procedure :: update_struct => update_struct

    end type
    
    interface mat3d_t
        module procedure mat3d_constructor
    end interface

    contains

    function mat3d_constructor(nx, ny, nz)
        type(mat3d_t), pointer :: mat3d_constructor

        allocate(mat3d_constructor)
        allocate(mat3d_constructor%matrix(0:nx,0:ny,0:nz))

    end function

    function get_item(this, material_type, i, j)
        class(mat3d_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8) :: get_item

        get_item = this%matrix(material_type, i, j)
    end function

    subroutine add_item(this, material_type, i, j, val)
        class(mat3d_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8) :: val

        this%matrix(material_type, i, j) = val
    end subroutine

    subroutine update_struct(this, ms, is, js, vals)
        class(mat3d_t) :: this
        integer, dimension(:), allocatable, intent(in) :: is, js, ms
        real(8), dimension(:), allocatable, intent(in) :: vals

        integer :: idx

        do idx=0, size(is) - 1
            this%matrix(ms(idx), is(idx), js(idx)) = vals(idx)
        end do
    end subroutine

    subroutine get_neighbors(this, material_type, i, j, curr_val, upper_val, lower_val, left_val, right_val)
        implicit none
        class(mat3d_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8), intent(out) :: curr_val, upper_val, lower_val, left_val, right_val

        curr_val = this%matrix(material_type, i, j)
        upper_val = this%matrix(material_type, i-1, j)
        lower_val = this%matrix(material_type, i+1, j)
        left_val = this%matrix(material_type, i, j-1)
        right_val = this%matrix(material_type, i, j+1)
    end subroutine

end module