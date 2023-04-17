module linked_list_module
    use link_module

    type, public :: linked_list_t
        private

        class(link_t), pointer, public :: head
        class(link_t), pointer, public :: tail

        contains

            procedure, public :: add
            procedure, public :: get

            procedure, public :: remove_mat
            procedure, public :: update_mat

    end type

    interface linked_list_t
        module procedure list_constructor
    end interface

    contains

    type(linked_list_t) function list_constructor()
        nullify(list_constructor%head)
        nullify(list_constructor%tail)
    end function


    subroutine remove_mat(this, mat)
        implicit none
        class(linked_list_t), intent(inout) :: this
        integer, intent(in) :: mat
        class(link_t), pointer :: temp_link, link_pointer

        temp_link => this%head
        if (.not. associated(temp_link)) then
            return
        else if (temp_link%material_type == mat) then
            this%head => this%head%get_next()
            deallocate(temp_link)
            return
        end if

        do while (associated(temp_link))
            if (temp_link%material_type == mat) then
                call link_pointer%set_next(temp_link%get_next())
                deallocate(temp_link)
                return
            end if

            link_pointer => temp_link
            temp_link => temp_link%get_next()
        end do

    end subroutine


    subroutine update_mat(this, mat, val)
        implicit none
        class(linked_list_t), intent(inout) :: this
        integer, intent(in) :: mat
        real(8), intent(in) :: val
        class(link_t), pointer :: temp_link, new_link

        temp_link => this%head

        do while (associated(temp_link))
            if (temp_link%material_type == mat) then
                temp_link%val = val
                return
            end if

            temp_link => temp_link%get_next()
        end do
        
        allocate(new_link)
        call new_link%link_init(mat, val)

        call this%add(new_link)
    end subroutine


    subroutine add(this, link)
        implicit none
        class(linked_list_t), intent(inout) :: this
        class(link_t), pointer :: link, temp_link

        if (.not. associated(this%head))  then
            this%head => link
            this%tail => link
        else 
            call this%tail%set_next(link)
            this%tail => link
        end if
    end subroutine


    function get(this, material_type) result(val)
        class(linked_list_t), intent(inout) :: this
        integer, intent(in) :: material_type
        real(8):: val
        class(link_t), pointer :: temp_link

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