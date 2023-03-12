module linked_list_module
    use link_module

    type, public :: linked_list_t
        private

        type(link_t), pointer, public :: head
        type(link_t), pointer, public :: tail

        contains

            procedure, public :: add
            procedure, public :: get

    end type

    interface linked_list_t
        module procedure list_constructor
    end interface

    contains

    type(linked_list_t) function list_constructor()
        nullify(list_constructor%head)
        nullify(list_constructor%tail)
    end function


    subroutine add(this, link)
        implicit none
        class(linked_list_t), intent(inout) :: this
        type(link_t), pointer :: link, temp_link

        if (.not. associated(this%head))  then
        ! print*, 'a'
            this%head => link
            this%tail => link
        else 
            ! print*, 'b'
            call this%tail%set_next(link)
            this%tail => link
        end if
        
        ! temp_link => this%head
        ! this%head => link

        ! call this%head%set_next(temp_link)
    end subroutine


    function get(this, material_type) result(val)
        class(linked_list_t), intent(inout) :: this
        integer, intent(in) :: material_type
        real(8):: val
        type(link_t), pointer :: temp_link

        temp_link => this%head

        do while (associated(temp_link))
            if (temp_link%material_type == material_type) then
                val = temp_link%val
                return
            end if

            temp_link => temp_link%get_next()
        end do

        val = 0
    end function get


end module