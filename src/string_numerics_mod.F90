module string_numerics_mod

  implicit none

  private

  public to_string
  public to_integer
  public to_float
  public to_double

  interface to_string
    module procedure integer1_to_string
    module procedure integer2_to_string
    module procedure integer4_to_string
    module procedure integer8_to_string
    module procedure integer4_array_to_string
    module procedure real4_to_string
    module procedure real8_to_string
    module procedure real8_array_to_string
    module procedure logical_to_string
  end interface to_string

contains

  pure function integer1_to_string(x) result(res)

    integer(1), intent(in) :: x
    character(:), allocatable :: res

    character(range(x)+2) tmp

    write(tmp, '(i0)') x
    res = trim(tmp)

  end function integer1_to_string
  
  pure function integer2_to_string(x) result(res)

    integer(2), intent(in) :: x
    character(:), allocatable :: res

    character(range(x)+2) tmp

    write(tmp, '(i0)') x
    res = trim(tmp)

  end function integer2_to_string
  
  pure function integer4_to_string(x) result(res)

    integer(4), intent(in) :: x
    character(:), allocatable :: res

    character(range(x)+2) tmp

    write(tmp, '(i0)') x
    res = trim(tmp)

  end function integer4_to_string
  
  pure function integer8_to_string(x) result(res)

    integer(8), intent(in) :: x
    character(:), allocatable :: res

    character(range(x)+2) tmp

    write(tmp, '(i0)') x
    res = trim(tmp)

  end function integer8_to_string

  pure function integer4_array_to_string(x) result(res)

    integer(4), intent(in) :: x(:)
    character(:), allocatable :: res

    character((range(x)+4) * size(x)) tmp
    character(256) fmt

    write(fmt, '("(", I0, "(I0, "", ""))")') size(x)
    write(tmp, fmt) x
    res = trim(tmp)

  end function integer4_array_to_string

  pure function real4_to_string(x, decimal_width, width) result(res)

    real(4), intent(in) :: x
    integer, intent(in) :: decimal_width
    integer, intent(in), optional :: width
    character(:), allocatable :: res

    integer w
    character(10) fmt
    character(range(x)+2) tmp

    if (present(width)) then
      w = max(width, decimal_width + 1 + 6)
    else
      w = decimal_width + 1 + 6
    end if
    write(fmt, "('(G', I0, '.', I0, ')')") w, decimal_width + 1
    write(tmp, fmt) x
    res = trim(adjustl(tmp))

  end function real4_to_string

  pure function real8_to_string(x, decimal_width, width) result(res)

    real(8), intent(in) :: x
    integer, intent(in) :: decimal_width
    integer, intent(in), optional :: width
    character(:), allocatable :: res

    integer w
    character(10) fmt
    character(range(x)+2) tmp

    if (present(width)) then
      w = max(width, decimal_width + 1 + 6)
    else
      w = decimal_width + 1 + 6
    end if
    write(fmt, "('(G', I0, '.', I0, ')')") w, decimal_width + 1
    write(tmp, fmt) x
    res = trim(adjustl(tmp))

  end function real8_to_string

  pure function real8_array_to_string(x, decimal_width, width) result(res)

    real(8), intent(in) :: x(:)
    integer, intent(in) :: decimal_width
    integer, intent(in), optional :: width
    character(:), allocatable :: res

    integer w, i, j
    character(256) s
    character(:), allocatable :: tmp

    if (present(width)) then
      w = max(width, decimal_width + 1 + 5)
    else
      w = decimal_width + 1 + 5
    end if
    allocate(character(2+(w+1)*size(x)-1)::tmp)
    tmp(2:len(tmp)) = ''
    tmp(1:1) = '['
    j = 2
    do i = 1, size(x)
      s = to_string(x(i), decimal_width, width)
      tmp(j:j+len_trim(s)-1) = trim(s)
      j = j + len_trim(s)
      if (i /= size(x)) then
        write(tmp(j:j), '(",")')
        j = j + 1
      end if
    end do
    tmp(j:j) = ']'
    res = trim(tmp)
    deallocate(tmp)

  end function real8_array_to_string

  pure function logical_to_string(x) result(res)

    logical, intent(in) :: x
    character(:), allocatable :: res

    res = trim(merge('true ', 'false', x))

  end function logical_to_string

  pure integer function to_integer(x) result(res)

    character(*), intent(in) :: x

    read(x, *) res

  end function to_integer

  pure real(4) function to_float(x) result(res)

    character(*), intent(in) :: x

    read(x, *) res

  end function to_float

  pure real(8) function to_double(x) result(res)

    character(*), intent(in) :: x

    read(x, *) res

  end function to_double

end module string_numerics_mod
