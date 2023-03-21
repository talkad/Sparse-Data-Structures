module dynamic_array_module

    type, public :: dynamic_array_t
        ! private
        real(8), dimension(:), allocatable :: values

        contains

            procedure, private, nopass :: append_real

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

    end function

    ! helper function - append val to array if it is not full, otherwise enlarge the array and append
    subroutine append_real(array, val, idx)
        implicit none
        real(8), dimension(:), allocatable, intent(inout) :: array
        real(8), intent(in) :: val
        integer, intent(in) :: idx  

        integer :: prev_size, new_size
        real(8), dimension(:), allocatable :: temp

        prev_size = size(array, dim=1)

        if (prev_size-1 <= idx) then
            new_size = prev_size!int(prev_size*1.5)-2
            allocate(temp(-1:new_size))                     ! enlarge array size by factor of 2
            temp(-1:new_size) = 0d0
            temp(-1:prev_size-2) = array(-1:prev_size-2)    ! copy previous values
            call move_alloc(temp, array)                    ! temp gets deallocated
        end if

        array(idx) = val
    end subroutine


    real(8) function get(this, idx)
        implicit none
        class(dynamic_array_t) :: this
        integer, intent(in) :: idx
        
        get = this%values(idx)
    end function


end module