module string_mod

  implicit none

  private

  public string_type
  public assignment(=)
  public operator(//)

  type string_type
    character(:), allocatable :: value
  contains
    procedure :: at => string_at
    procedure :: len => string_len
    final :: string_final
  end type string_type

  interface assignment(=)
    module procedure string_assign_lhs
    module procedure string_assign_rhs
  end interface assignment(=)

  interface operator(//)
    module procedure string_cat_lhs
    module procedure string_cat_rhs
  end interface operator(//)

contains

  pure subroutine string_assign_lhs(this, str)

    type(string_type), intent(inout) :: this
    character(*), intent(in) :: str

    if (allocated(this%value)) deallocate(this%value)
    this%value = str

  end subroutine string_assign_lhs

  pure subroutine string_assign_rhs(str, this)

    character(:), intent(out), allocatable :: str
    type(string_type), intent(in) :: this

    str = this%value

  end subroutine string_assign_rhs

  pure function string_cat_lhs(this, str) result(res)

    type(string_type), intent(in) :: this
    character(*), intent(in) :: str
    character(:), allocatable :: res

    res = trim(this%value) // trim(str)

  end function string_cat_lhs

  pure function string_cat_rhs(str, this) result(res)

    character(*), intent(in) :: str
    type(string_type), intent(in) :: this
    character(:), allocatable :: res

    res = trim(str) // trim(this%value)

  end function string_cat_rhs

  pure function string_at(this, start_pos, end_pos) result(res)

    class(string_type), intent(in) :: this
    integer, intent(in) :: start_pos
    integer, intent(in), optional :: end_pos
    character(:), allocatable :: res

    if (present(end_pos)) then
      res = this%value(start_pos:end_pos)
    else
      res = this%value(start_pos:start_pos)
    end if

  end function string_at

  pure integer function string_len(this) result(res)

    class(string_type), intent(in) :: this

    if (allocated(this%value)) then
      res = len_trim(this%value)
    else
      res = 0
    end if

  end function string_len

  subroutine string_final(this)

    type(string_type), intent(inout) :: this

    if (allocated(this%value)) deallocate(this%value)

  end subroutine string_final

end module string_mod
