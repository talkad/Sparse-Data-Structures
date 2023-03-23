module dynamic_array_module

    type, public :: dynamic_array_t
        ! private
        real(8), dimension(:), allocatable :: values
        integer :: nz_count

        contains

            procedure, private :: append_real

            ! procedure, public :: add
            procedure, public :: get

    end type

    interface dynamic_array_t
        module procedure dynamic_array_constructor
    end interface

    contains

    function dynamic_array_constructor()
        implicit none
        type(dynamic_array_t), pointer :: dynamic_array_constructor
        
        allocate(dynamic_array_constructor)
        allocate(dynamic_array_constructor%values(-1:0))

        dynamic_array_constructor%values(-1:0) = 0d0
        dynamic_array_constructor%nz_count = 0

    end function

    ! helper function - append val to array if it is not full, otherwise enlarge the array and append
    subroutine append_real(this, val)
        implicit none
        class(dynamic_array_t) :: this
        real(8), intent(in) :: val

        integer :: prev_size, new_size
        real(8), dimension(:), allocatable :: temp

        prev_size = size(this%values, dim=1)

        if (prev_size-1 <= this%nz_count) then
            new_size = prev_size!int(prev_size*1.5)-2
            allocate(temp(-1:new_size))                     ! enlarge array size by factor of 2
            temp(-1:new_size) = 0d0
            temp(-1:prev_size-2) = this%values(-1:prev_size-2)    ! copy previous values
            call move_alloc(temp, this%values)                    ! temp gets deallocated
        end if

        this%values(this%nz_count) = val
        this%nz_count = this%nz_count + 1
    end subroutine


    real(8) function get(this, idx)
        implicit none
        class(dynamic_array_t) :: this
        integer, intent(in) :: idx
        
        get = this%values(idx)
    end function


end module