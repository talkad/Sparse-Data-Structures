module dynamic_array_module

    type, public :: dynamic_array_t
        ! private
        real(8), dimension(:), allocatable :: values
        integer, dimension(:), allocatable :: types

        integer :: upper_bound

        contains

            procedure, private, nopass :: append_real
            procedure, private, nopass :: append_int

            ! procedure, public :: add
            procedure, public :: get

    end type

    interface dynamic_array_t
        module procedure dynamic_array_constructor
    end interface

    contains

    function dynamic_array_constructor(array)
        implicit none
        type(dynamic_array_t), pointer :: dynamic_array_constructor
        real(8), dimension(:), intent(in) :: array
        integer :: m, idx
        logical :: start_col = .False.
        integer :: mapping_length, nx, ny, materials
        
        allocate(dynamic_array_constructor)
        idx = 0
        
        do m = 1, size(array, dim=1)
            if (array(m) > 0) then
                call dynamic_array_constructor%append_real(dynamic_array_constructor%values, array(m), idx)
                call dynamic_array_constructor%append_int(dynamic_array_constructor%types, m-1, idx)

                idx = idx + 1
            end if
        end do

        dynamic_array_constructor%upper_bound = idx - 1

    end function

    ! helper function - append val to array if it is not full, otherwise enlarge the array and append
    subroutine append_real(array, val, idx)
        implicit none
        real(8), dimension(:), allocatable, intent(inout) :: array
        real(8), intent(in) :: val
        integer, intent(in) :: idx

        integer :: prev_size
        real(8), dimension(:), allocatable :: temp

        if (allocated(array)) then
            prev_size = size(array, dim=1)
        end if

        if (.not. allocated(array)) then
            allocate(array(0:1))
        else if (prev_size <= idx) then
            allocate(temp(0:prev_size*2))            ! enlarge array size by factor of 2
            temp(0:prev_size*2) = 0d0
            temp(0:prev_size) = array(0:prev_size)   ! copy previous values
            call move_alloc(temp, array)             ! temp gets deallocated
            ! deallocate(array)
            ! array=temp
        end if

        array(idx) = val
    end subroutine


    subroutine append_int(array, val, idx)
        ! temporal solution
        implicit none
        integer, dimension(:), allocatable, intent(inout) :: array
        integer, intent(in) :: val
        integer, intent(in) :: idx

        integer :: prev_size
        integer, dimension(:), allocatable :: temp

        if (allocated(array)) then
            prev_size = size(array, dim=1)
        end if

        if (.not. allocated(array)) then
            allocate(array(0:3))
        else if (prev_size <= idx) then
            allocate(temp(0:prev_size*2))            ! enlarge array size by factor of 2
            temp(0:prev_size*2) = 0d0
            temp(0:prev_size) = array(0:prev_size)   ! copy previous values
            call move_alloc(temp, array)             ! temp gets deallocated
        end if

        array(idx) = val
    end subroutine



    real(8) function get(this, material_type)
        implicit none
        class(dynamic_array_t) :: this
        integer, intent(in) :: material_type
        integer :: i 

        do i = 0, size(this%types, dim=1)
            if (this%types(i) == material_type) then
                get = this%values(i)
                return
            end if
        end do
        
        get = 0
    end function


end module