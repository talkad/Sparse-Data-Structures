module mat3d_module
    use data_abstract_module

    type, extends(data_abstract_t) :: mat3d_t
        real(8), dimension(:,:,:), allocatable :: matrix
        contains
            procedure :: get_item => get_item
            procedure :: add_item => add_item

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

end module