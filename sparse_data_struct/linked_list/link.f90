module link_module

    type, public :: link_t

        integer :: material_type
        real(8) :: val
        type(link_t), pointer :: next

        contains

            procedure, public :: link_init
            procedure, public :: get_next
            procedure, public :: set_next

    end type

    contains

    subroutine link_init(this, material_type, val)
        implicit none

        class(link_t), intent(inout) :: this
        integer, intent(in) :: material_type
        real(8), intent(in) :: val

        this%material_type = material_type
        this%val = val
    end subroutine link_init


    subroutine set_next(this, next)
        implicit none

        class(link_t), intent(inout) :: this
        type(link_t), target, intent(in) :: next

        this%next => next
    end subroutine


    function get_next(this) result(next)
        class(link_t), intent(inout) :: this
        class(link_t), pointer:: next

        next => this%next
    end function get_next

end module